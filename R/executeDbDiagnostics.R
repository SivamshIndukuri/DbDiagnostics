# @file executeDbDiagnostics.R
#
# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of the DbProfile package
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' executeDbDiagnostics
#'
#' @param connectionDetails         	  A connectionDetails object for connecting to the database containing the DbProfile results
#' @param resultsDatabaseSchema     	  The fully qualified database name of the results schema where the DbProfile results are housed. Default is "dp_temp".
#' @param resultsTableName						  The name of the table in the results schema with the DbProfile results. Default is "dp_achilles_results_augmented."
#' @param outputFolder              	  Results will be written to this directory, default = getwd()
#' @param dataDiagnosticsSettingsList		A list of settings objects, each created from DataDiagnostics::createDataDiagnosticsSettings() function and each representing one analysis.
#'
#' @import DataQualityDashboard Achilles DatabaseConnector SqlRender dplyr magrittr
#' @importFrom ParallelLogger saveSettingsToJson
#'
#' @export

executeDbDiagnostics <- function(connectionDetails,
                                 resultsDatabaseSchema,
                                 resultsTableName,
                                 outputFolder = getwd(),
                                 dataDiagnosticsSettingsList) {
  # Set up outputFolder

  if (!dir.exists(outputFolder)) {
    dir.create(path = outputFolder, recursive = TRUE)
  }

  # Connect to the results schema to get list of databases included in results table ---------------
  options(scipen = 999)

  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  # TODO check the name of the column in the results schema -----------
  sql <- "SELECT DISTINCT CDM_SOURCE_NAME, RELEASE_KEY
        FROM @results_database_schema.@results_table_name"

  rsql <- SqlRender::render(
    sql = sql,
    results_database_schema = resultsDatabaseSchema,
    results_table_name = resultsTableName
  )
  tsql <- SqlRender::translate(rsql, connectionDetails$dbms)

  dbNames <- DatabaseConnector::querySql(conn, tsql)

  # Get the most recent release for each database -------------------------------
  # message("Get most recent database release")
  # for(i in 1:nrow(dbNames)){
  # 	dbInfo <- strsplit(dbNames$DB_ID[i], split = "-")
  #
  # 	dbNames$DB_ABBREV[i] <- dbInfo[[1]][[1]]
  # 	dbNames$DB_DATE[i] <- dbInfo[[1]][[2]]
  #
  # 	rm(dbInfo)
  # }
  # rm(i)
  #
  # latestDbs<- dbNames %>%
  # 	group_by(DB_ABBREV) %>%
  # 	slice_max(DB_DATE, n=1)


  # refactoring portion ------------------------------------------------------
  # concept Ids
  sql <- "SELECT analysis_id, stratum_1, release_key
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id IN (4, 5)

        UNION ALL

        SELECT analysis_id, MIN(stratum_1) AS stratum_1, release_key
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id IN (101, 111, 112)
        GROUP BY analysis_id, release_key

        UNION ALL

        SELECT analysis_id, MAX(stratum_1) AS stratum_1, release_key
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id IN (101, 111, 112)
        GROUP BY analysis_id, release_key"

  rsql <- SqlRender::render(sql,
    results_database_schema = resultsDatabaseSchema,
    results_table_name = resultsTableName
  )

  tsql <- SqlRender::translate(rsql, targetDialect = connectionDetails$dbms)

  conceptIdsTable <- DatabaseConnector::querySql(conn, tsql)

  dbNum <- nrow(dbNames)

  for (i in 1:dbNum) {
    dbName <- dbNames[i, 2]

    message(paste0("Database: ", dbName, " (", i, "/", nrow(dbNames), ")"))

    dbProfile <- conceptIdsTable %>% filter(release_key == dbName)
    names(dbProfile) <- toupper(names(dbProfile))

    for (k in 1:length(dataDiagnosticsSettingsList)) {
      studySpecs <- dataDiagnosticsSettingsList[[k]]

      # TODO ---------------
      # evaluate the specs input
      # look at data types and stop if target concept id is null
      if (is.null(studySpecs$targetConceptIds)) {
        stop("Need to specify targetConceptIds")
      }

      checkmate::assertInteger(studySpecs$analysisId, null.ok = FALSE, len = 1)
      checkmate::assertString(studySpecs$analysisName, null.ok = FALSE, min.chars = 1)
      checkmate::assertNumeric(studySpecs$minAge, null.ok = TRUE)
      checkmate::assertNumeric(studySpecs$maxAge, null.ok = TRUE)
      checkmate::assertIntegerish(studySpecs$genderConceptIds, null.ok = FALSE)
      checkmate::assertIntegerish(studySpecs$raceConceptIds, null.ok = TRUE)
      checkmate::assertIntegerish(studySpecs$ethnicityConceptIds, null.ok = TRUE)
      checkmate::assertString(studySpecs$studyStartDate, null.ok = FALSE, min.chars = 6, max.chars = 6)
      checkmate::assertString(studySpecs$studyEndDate, null.ok = FALSE, min.chars = 6, max.chars = 6)
      checkmate::assertInteger(studySpecs$requiredDurationDays, null.ok = FALSE)

      allowed_visits <- c("IP", "OP", "ER")
      checkmate::assertSubset(studySpecs$requiredVisits, choices = allowed_visits, empty.ok = FALSE, null.ok = TRUE)
      checkmate::assertSubset(studySpecs$desiredVisits, choices = allowed_visits, empty.ok = FALSE, null.ok = TRUE)

      checkmate::assertString(studySpecs$targetName, null.ok = FALSE)
      checkmate::assertIntegerish(studySpecs$targetConceptIds, null.ok = FALSE, min.len = 1)
      checkmate::assertString(studySpecs$comparatorName, null.ok = TRUE)
      checkmate::assertIntegerish(studySpecs$comparatorConceptIds, null.ok = TRUE)
      checkmate::assertString(studySpecs$indicationName, null.ok = TRUE)
      checkmate::assertIntegerish(studySpecs$indicationConceptIds, null.ok = TRUE)
      checkmate::assertLogical(studySpecs$includeIndicationInCalc, null.ok = FALSE)
      checkmate::assertString(studySpecs$outcomeName, null.ok = TRUE)
      checkmate::assertIntegerish(studySpecs$outcomeConceptIds, null.ok = TRUE)
      # ---------------

      ddThresholds <- read.csv(system.file("csv", "ddThresholds.csv", package = "DbDiagnostics"), stringsAsFactors = FALSE)

      # ID of this individual study
      analysisId <- studySpecs$analysisId

      # Name of this individual study
      analysisName <- studySpecs$analysisName

      message(paste0("   -- Analysis #", analysisId, " - ", analysisName, " (", k, "/", length(dataDiagnosticsSettingsList), ")"))

      numCriteria <- 0

      # Age
      if (is.null(studySpecs$minAge)) {
        minAge <- min(as.integer(dbProfile[which(dbProfile$ANALYSIS_ID == 101), ]$STRATUM_1))
      } else {
        minAge <- studySpecs$minAge
        numCriteria <- numCriteria + 1
      }

      if (is.null(studySpecs$maxAge)) {
        maxAge <- max(as.integer(dbProfile[which(dbProfile$ANALYSIS_ID == 101), ]$STRATUM_1))
      } else {
        maxAge <- studySpecs$maxAge
        numCriteria <- numCriteria + 1
      }

      maxYearInDb <- as.integer(substr(max(dbProfile[which(dbProfile$ANALYSIS_ID == 111), ]$STRATUM_1), 1, 4))
      minYearInDb <- as.integer(substr(min(dbProfile[which(dbProfile$ANALYSIS_ID == 111), ]$STRATUM_1), 1, 4))

      minBirthYearNeeded <- minYearInDb - maxAge
      maxBirthYearNeeded <- maxYearInDb - minAge

      ageSpec <- if (!is.null(studySpecs$maxAge) && !is.null(studySpecs$minAge)) {
        paste("age", studySpecs$minAge, "- age", studySpecs$maxAge)
      } else if (is.null(studySpecs$maxAge) && !is.null(studySpecs$minAge)) {
        paste("> age", studySpecs$minAge)
      } else if (!is.null(studySpecs$maxAge) && is.null(studySpecs$minAge)) {
        paste("< age", studySpecs$maxAge)
      } else {
        paste("age", minAge, "- age", maxAge)
      }

      # Gender
      genderConceptIds <- studySpecs$genderConceptIds # Q - limit to these two or to all genders in the db? Means including 0
      numCriteria <- numCriteria + 1

      # Race
      if (is.null(studySpecs$raceConceptIds)) {
        raceConceptIds <- dbProfile %>%
          filter(ANALYSIS_ID == 4) %>%
          select(STRATUM_1) %>%
          .[["STRATUM_1"]]
      } else {
        raceConceptIds <- studySpecs$raceConceptIds
        numCriteria <- numCriteria + 1
      }

      # Ethnicity
      if (is.null(studySpecs$ethnicityConceptIds)) {
        ethnicityConceptIds <- dbProfile %>%
          filter(ANALYSIS_ID == 5) %>%
          select(STRATUM_1) %>%
          .[["STRATUM_1"]]
      } else {
        ethnicityConceptIds <- studySpecs$ethnicityConceptIds
        numCriteria <- numCriteria + 1
      }

      # Study Start Date
      if (is.null(studySpecs$studyStartDate)) {
        studyStartDate <- min(dbProfile[which(dbProfile$ANALYSIS_ID == 111), ]$STRATUM_1)
      } else {
        studyStartDate <- max(as.numeric(studySpecs$studyStartDate), min(dbProfile[which(dbProfile$ANALYSIS_ID == 111), ]$STRATUM_1))
        numCriteria <- numCriteria + 1
      }

      # Study End Date
      if (is.null(studySpecs$studyEndDate)) {
        studyEndDate <- max(dbProfile[which(dbProfile$ANALYSIS_ID == 112), ]$STRATUM_1)
      } else {
        studyEndDate <- min(as.numeric(studySpecs$studyEndDate), max(dbProfile[which(dbProfile$ANALYSIS_ID == 111), ]$STRATUM_1))
        numCriteria <- numCriteria + 1
      }

      # Required follow-up time
      requiredDurationDays <- studySpecs$requiredDurationDays
      numCriteria <- numCriteria + 1

      # Required domains
      requiredDomains <- studySpecs$requiredDomains
      numCriteria <- numCriteria + 1

      if ("condition" %in% requiredDomains) {
        requiredCondition <- 1
      } else {
        requiredCondition <- 0
      }
      if ("drug" %in% requiredDomains) {
        requiredDrug <- 1
      } else {
        requiredDrug <- 0
      }
      if ("device" %in% requiredDomains) {
        requiredDevice <- 1
      } else {
        requiredDevice <- 0
      }
      if ("measurement" %in% requiredDomains) {
        requiredMeasurement <- 1
      } else {
        requiredMeasurement <- 0
      }
      if ("procedure" %in% requiredDomains) {
        requiredProcedure <- 1
      } else {
        requiredProcedure <- 0
      }
      if ("observation" %in% requiredDomains) {
        requiredObservation <- 1
      } else {
        requiredObservation <- 0
      }

      bitString <- paste0(requiredCondition, requiredDrug, requiredDevice, requiredMeasurement, 0, requiredProcedure, requiredObservation)

      # Desired domains
      desiredDomains <- studySpecs$desiredDomains

      if ("condition" %in% desiredDomains) {
        desiredCondition <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredCondition <- 0
      }
      if ("drug" %in% desiredDomains) {
        desiredDrug <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredDrug <- 0
      }
      if ("device" %in% desiredDomains) {
        desiredDevice <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredDevice <- 0
      }
      if ("measurement" %in% desiredDomains) {
        desiredMeasurement <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredMeasurement <- 0
      }
      if ("procedure" %in% desiredDomains) {
        desiredProcedure <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredProcedure <- 0
      }
      if ("observation" %in% desiredDomains) {
        desiredObservation <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredObservation <- 0
      }
      if ("measurementValues" %in% desiredDomains) {
        desiredMeasurementValues <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredMeasurementValues <- 0
      }
      if ("death" %in% desiredDomains) {
        desiredDeath <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredDeath <- 0
      }

      # Required visits
      requiredVisits <- studySpecs$requiredVisits

      if ("IP" %in% requiredVisits) {
        requiredIP <- 1
      } else {
        requiredIP <- 0
      }
      if ("OP" %in% requiredVisits) {
        requiredOP <- 1
      } else {
        requiredOP <- 0
      }
      if ("ER" %in% requiredVisits) {
        requiredER <- 1
      } else {
        requiredER <- 0
      }

      # Desired visits
      desiredVisits <- studySpecs$desiredVisits

      if ("IP" %in% desiredVisits) {
        desiredIP <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredIP <- 0
      }
      if ("OP" %in% desiredVisits) {
        desiredOP <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredOP <- 0
      }
      if ("ER" %in% desiredVisits) {
        desiredER <- 1
        numCriteria <- numCriteria + 1
      } else {
        desiredER <- 0
      }

      # target
      target <- studySpecs$targetName
      requiredTargetConcepts <- studySpecs$targetConceptIds
      numCriteria <- numCriteria + 1

      # comparator
      if (!is.null(studySpecs$comparatorConceptIds)) {
        comparator <- studySpecs$comparatorName
        requiredComparatorConcepts <- studySpecs$comparatorConceptIds
        numCriteria <- numCriteria + 1
      } else {
        comparator <- ""
        requiredComparatorConcepts <- ""
      }

      # indication
      if (!is.null(studySpecs$indicationConceptIds)) {
        indication <- studySpecs$indicationName
        requiredIndicationConcepts <- studySpecs$indicationConceptIds
        numCriteria <- numCriteria + 1
      } else {
        indication <- ""
        requiredIndicationConcepts <- ""
      }

      # outcome
      if (!is.null(studySpecs$outcomeConceptIds)) {
        outcome <- studySpecs$outcomeName
        requiredOutcomeConcepts <- studySpecs$outcomeConceptIds
        numCriteria <- numCriteria + 1
      } else {
        outcome <- ""
        requiredOutcomeConcepts <- ""
      }

      sql <- "
  	  -- Age
      SELECT
        count_value,
        'propInAgeRange' AS statistic,
        '@age_spec' AS spec,
        1 AS evaluate_threshold
      FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 3
        AND release_key = '@databaseName'
        AND CAST(stratum_1 AS INT) BETWEEN @min_birth_year_needed AND @max_birth_year_needed

      UNION ALL

      SELECT
        count_value,
        'propWithAgeAtFirstObs' AS statistic,
        '@age_spec' AS spec,
        1 AS evaluate_threshold
      FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 101
        AND release_key = '@databaseName'
        AND CAST(stratum_1 AS INT) BETWEEN @min_age AND @max_age

      UNION ALL
      -- Gender
      SELECT
        count_value,
        'propWithGenderCriteria' AS statistic,
        '@genderConceptIds' AS spec,
        1 AS evaluate_threshold
      FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 2
        AND release_key = '@databaseName'
        AND stratum_1 IN (@genderConceptIds)

      UNION ALL
      -- Race
      SELECT
        count_value,
        'propWithRaceCriteria' AS statistic,
        '@raceConceptIds' as spec,
        1 AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 4
        AND release_key = '@databaseName'
        AND stratum_1 IN (@raceConceptIds)

      UNION ALL
        -- Ethinicty
        SELECT
          count_value,
          'propWithEthnicityCriteria' AS statistic,
          '@ethnicityConceptIds' as spec,
          1 AS evaluate_threshold
          FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 5
          AND release_key = '@databaseName'
          AND stratum_1 IN (@ethnicityConceptIds)

      UNION ALL
        -- Longitudinality
        SELECT
          count_value,
          'propWithLongitudinalCriteria' AS statistic,
          '@requiredDurationDays days' AS spec,
          1 AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 108
          AND release_key = '@databaseName'
          AND stratum_1 >= ROUND(@requiredDurationDays / 30.0, 0)

      UNION ALL
        -- Data Domain Coverage
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithRequiredDomain' AS statistic,
          '@requiredDomains' AS spec,
          1 AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '@bitString'

      UNION ALL
        -- Conditions
        SELECT
        COALESCE(MAX(COUNT_VALUE), 0) AS count_value,
        'propWithConditionCriteria' AS statistic,
        CASE
          WHEN @desiredCondition = 1 THEN 'Conditions desired'
          WHEN @desiredCondition = 0 THEN 'Conditions not desired'
        END AS spec,
        @desiredCondition AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '1000000'

      UNION ALL
        -- Drugs
        SELECT
        COALESCE(MAX(COUNT_VALUE), 0) AS count_value,
        'propWithDrugCriteria' AS statistic,
        CASE
          WHEN @desiredDrug = 1 THEN 'Drugs desired'
          WHEN @desiredDrug = 0 THEN 'Drugs not desired'
        END AS spec,
        @desiredDrug AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '0100000'

      UNION ALL
        -- Device
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithDeviceCriteria' AS statistic,
          CASE
            WHEN @desiredDevice = 1 THEN 'Devices desired'
            WHEN @desiredDevice = 0 THEN 'Devices not desired'
          END AS spec,
          @desiredDevice AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '0010000'

      UNION ALL
        -- Measurement
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithMeasurementCriteria' AS statistic,
          CASE
            WHEN @desiredMeasurement = 1 THEN 'Measurements desired'
            WHEN @desiredMeasurement = 0 THEN 'Measurements not desired'
          END AS spec,
          @desiredMeasurement AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '0001000'

      UNION ALL
        -- Death
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithDeathCriteria' AS statistic,
          CASE
            WHEN @desiredDeath = 1 THEN 'Death domain desired'
            WHEN @desiredDeath = 0 THEN 'Death domain not desired'
          END AS spec,
          @desiredDeath AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '0000100'

      UNION ALL
        -- Procedure
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithProcedureCriteria' AS statistic,
          CASE
            WHEN @desiredProcedure = 1 THEN 'Procedures desired'
            WHEN @desiredProcedure = 0 THEN 'Procedures not desired'
          END AS spec,
          @desiredProcedure AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '0000010'

      UNION ALL
        -- Observation
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithObservationCriteria' AS statistic,
          CASE
            WHEN @desiredObservation = 1 THEN 'Observations desired'
            WHEN @desiredObservation = 0 THEN 'Observations not desired'
          END AS spec,
          @desiredObservation AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 2004
          AND release_key = '@databaseName'
          AND stratum_1 = '0000001'

      UNION ALL
        -- Inpatient Visit Criteria
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithIPCriteria' AS statistic,
          CASE
            WHEN @requiredIP = 1 AND @desiredIP = 1 THEN 'Inpatient visits required and desired'
            WHEN @requiredIP = 1 AND @desiredIP = 0 THEN 'Inpatient visits required'
            WHEN @requiredIP = 0 AND @desiredIP = 1 THEN 'Inpatient visits desired'
            WHEN @requiredIP = 0 AND @desiredIP = 0 THEN 'Inpatient visits not required nor desired'
          END AS spec,
          CASE
            WHEN @requiredIP = 1 AND @desiredIP = 1 THEN 1
            WHEN @requiredIP = 1 AND @desiredIP = 0 THEN 1
            WHEN @requiredIP = 0 AND @desiredIP = 1 THEN 3
            WHEN @requiredIP = 0 AND @desiredIP = 0 THEN 0
          END AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 200
          AND release_key = '@databaseName'
          AND visit_ancestor_concept_id IN (9201, 262)

      UNION ALL
        -- Outpatient Visit Criteria
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithOPCriteria' AS statistic,
          CASE
            WHEN @requiredOP = 1 AND @desiredOP = 1 THEN 'Outpatient visits required and desired'
            WHEN @requiredOP = 1 AND @desiredOP = 0 THEN 'Outpatient visits required'
            WHEN @requiredOP = 0 AND @desiredOP = 1 THEN 'Outpatient visits desired'
            WHEN @requiredOP = 0 AND @desiredOP = 0 THEN 'Outpatient visits not required nor desired'
          END AS spec,
          CASE
            WHEN @requiredOP = 1 AND @desiredOP = 1 THEN 1
            WHEN @requiredOP = 1 AND @desiredOP = 0 THEN 1
            WHEN @requiredOP = 0 AND @desiredOP = 1 THEN 3
            WHEN @requiredOP = 0 AND @desiredOP = 0 THEN 0
          END AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 200
          AND release_key = '@databaseName'
          AND visit_ancestor_concept_id IN (9202, 5083)

      UNION ALL
        -- Emergency Room Visit Criteria
        SELECT
          COALESCE(MAX(count_value), 0) AS count_value,
          'propWithERCriteria' AS statistic,
          CASE
            WHEN @requiredER = 1 AND @desiredER = 1 THEN 'Emergency Room visits required and desired'
            WHEN @requiredER = 1 AND @desiredER = 0 THEN 'Emergency Room visits required'
            WHEN @requiredER = 0 AND @desiredER = 1 THEN 'Emergency Room visits desired'
            WHEN @requiredER = 0 AND @desiredER = 0 THEN 'Emergency Room visits not required nor desired'
          END AS spec,
          CASE
            WHEN @requiredER = 1 AND @desiredER = 1 THEN 1
            WHEN @requiredER = 1 AND @desiredER = 0 THEN 1
            WHEN @requiredER = 0 AND @desiredER = 1 THEN 3
            WHEN @requiredER = 0 AND @desiredER = 0 THEN 0
          END AS evaluate_threshold
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id = 200
          AND release_key = '@databaseName'
          AND visit_ancestor_concept_id IN (9203, 262)

        UNION ALL
          -- Required Concepts
          SELECT
            COALESCE(MAX(count_value), 0) AS count_value,
            'propWithRequiredTargetConcepts' AS statistic,
            '@target' AS spec,
            2 AS evaluate_threshold
          FROM @results_database_schema.@results_table_name
          WHERE analysis_id IN (1800, 400, 600, 700, 800, 2100)
            AND release_key = '@databaseName'
            AND stratum_1 IN (@requiredTargetConcepts)


        UNION ALL
          {@requiredComparatorConcepts == ''} ? {
            SELECT
              -1 AS count_value,
              'propWithRequiredComparatorConcepts' AS statistic,
              'NA' AS spec,
              0 AS evaluate_threshold
          } : {
            SELECT
              COALESCE(MAX(count_value), 0) AS count_value,
              'propWithRequiredComparatorConcepts' AS statistic,
              '@comparator' AS spec,
              2 AS evaluate_threshold
            FROM @results_database_schema.@results_table_name
            WHERE analysis_id IN (1800, 400, 600, 700, 800, 2100)
              AND release_key = '@databaseName'
              AND stratum_1 IN (@requiredComparatorConcepts)
          }

        UNION ALL
          {@requiredIndicationConcepts == ''} ? {
          SELECT
            -1 AS count_value,
            'propWithRequiredIndicationConcepts' AS statistic,
            'NA' AS spec,
            0 AS evaluate_threshold
          } : {
            SELECT
              COALESCE(MAX(count_value), 0) AS count_value,
              'propWithRequiredIndicationConcepts' AS statistic,
              '@indication' AS spec,
              2 AS evaluate_threshold
            FROM @results_database_schema.@results_table_name
            WHERE analysis_id IN (1800, 400, 600, 700, 800, 2100)
              AND release_key = '@databaseName'
              AND stratum_1 IN (@requiredIndicationConcepts)
          }

          UNION ALL
            {@requiredOutcomeConcepts == ''} ? {
              SELECT
                -1 AS count_value,
                'propWithRequiredOutcomeConcepts' AS statistic,
                'NA' AS spec,
                0 AS evaluate_threshold
            } : {
              SELECT
                -1 AS count_value,
                'propWithRequiredOutcomeConcepts' AS statistic,
                '@outcome' AS spec,
                2 AS evaluate_threshold
              FROM @results_database_schema.@results_table_name
              WHERE analysis_id IN (1800, 400, 600, 700, 800, 2100)
                AND release_key = '@databaseName'
                AND stratum_1 IN (@requiredOutcomeConcepts)
          }
      "

      rsql <- SqlRender::render(
        sql = sql,
        results_database_schema = resultsDatabaseSchema,
        results_table_name = resultsTableName,
        databaseName = dbName,
        age_spec = ageSpec,
        min_birth_year_needed = minBirthYearNeeded,
        max_birth_year_needed = maxBirthYearNeeded,
        min_age = minAge,
        max_age = maxAge,
        genderConceptIds = genderConceptIds,
        raceConceptIds = raceConceptIds,
        ethnicityConceptIds = ethnicityConceptIds,
        requiredDurationDays = requiredDurationDays,
        bitString = bitString,
        requiredDomains = paste(requiredDomains, collapse = ", "),
        desiredCondition = desiredCondition,
        desiredDrug = desiredDrug,
        desiredDevice = desiredDevice,
        desiredMeasurement = desiredMeasurement,
        desiredDeath = desiredDeath,
        desiredProcedure = desiredProcedure,
        desiredObservation = desiredObservation,
        requiredIP = requiredIP,
        desiredIP = desiredIP,
        requiredOP = requiredOP,
        desiredOP = desiredOP,
        requiredER = requiredER,
        desiredER = desiredER,
        target = target,
        requiredTargetConcepts = requiredTargetConcepts,
        comparator = comparator,
        requiredComparatorConcepts = requiredComparatorConcepts,
        indication = indication,
        requiredIndicationConcepts = requiredIndicationConcepts,
        outcome = outcome,
        requiredOutcomeConcepts = requiredOutcomeConcepts
      )

      tsql <- SqlRender::translate(rsql, connectionDetails$dbms)

      personOutput <- DatabaseConnector::querySql(conn, tsql, snakeCaseToCamelCase = TRUE)

      personOutput$countValue[personOutput$countValue == -1] <- NA
      personOutput$spec[personOutput$spec == "NA"] <- NA

      # Calendar Time ----------

      sql <- "
      SELECT
        'numPersonsInDb' AS stat_name,
        COALESCE(MAX(count_value), 0) AS counts
      FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 1
        AND release_key = '@databaseName'

      UNION ALL

      SELECT
        'totalObsPeriods' AS stat_name,
        COALESCE(SUM(count_value), 0) AS counts
      FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 111
        AND release_key = '@databaseName'

      UNION ALL

      SELECT
        'obs_starts' AS stat_name,
        COALESCE(SUM(count_value), 0) AS counts
      FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 111
        AND release_key = '@databaseName'
        AND stratum_1 <= '@studyEndDate'

      UNION ALL

      SELECT
        'obs_ends' AS stat_name,
        COALESCE(SUM(count_value), 0) AS counts
      FROM @results_database_schema.@results_table_name
      WHERE analysis_id = 112
        AND release_key = '@databaseName'
        AND stratum_1 >= '@studyStartDate'
    "

      calendarStats <- DatabaseConnector::renderTranslateQuerySql(
        connection = conn,
        sql = sql,
        snakeCaseToCamelCase = TRUE,
        dbms = connectionDetails$dbms,
        results_database_schema = resultsDatabaseSchema,
        results_table_name = resultsTableName,
        databaseName = dbName,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate
      )


      numPersonsInDb <- calendarStats$counts[calendarStats$statName == "numPersonsInDb"]

      totalObsPeriods <- calendarStats$counts[calendarStats$statName == "totalObsPeriods"]

      avgObsPeriodsPerPerson <- totalObsPeriods / numPersonsInDb

      obsStartsCount <- calendarStats$counts[calendarStats$statName == "obs_starts"]

      personsWithCalendarStarts <- obsStartsCount / avgObsPeriodsPerPerson

      propWithCalendarStarts <- personsWithCalendarStarts / numPersonsInDb

      obsEndsCount <- calendarStats$counts[calendarStats$statName == "obs_ends"]

      personsWithCalendarEnds <- obsEndsCount / avgObsPeriodsPerPerson

      propWithCalendarEnds <- personsWithCalendarEnds / numPersonsInDb

      calendarTime <- (1 - ((1 - propWithCalendarEnds) + (1 - propWithCalendarStarts)))

      numPersonsWithCalendarTime <- calendarTime * numPersonsInDb

      personsWithCalendarTime <- as.data.frame(cbind(
        "propWithCalendarTime",
        numPersonsWithCalendarTime,
        calendarTime
      )) %>%
        rename(
          "statistic" = "V1",
          "value" = "numPersonsWithCalendarTime",
          "proportion" = "calendarTime"
        ) %>%
        mutate(
          spec = paste(studyStartDate, studyEndDate, sep = "-"),
          evaluateThreshold = 1
        )

      # Data Domain Coverage - Measurements w/Values

      sql <- "
        SELECT
          COALESCE(SUM(CASE WHEN analysis_id = 1801 THEN count_value END), 0) AS num_meas_records,
          COALESCE(MAX(CASE WHEN analysis_id = 1814 THEN count_value END), 0) AS num_meas_records_with_values
        FROM @results_database_schema.@results_table_name
        WHERE analysis_id IN (1801, 1814)
          AND release_key = '@databaseName'
      "

      numMeasurementsTable <- DatabaseConnector::renderTranslateQuerySql(
        connection = conn,
        sql = sql,
        snakeCaseToCamelCase = TRUE,
        dbms = connectionDetails$dbms,
        results_database_schema = resultsDatabaseSchema,
        results_table_name = resultsTableName,
        databaseName = dbName
      )


      numMeasRecords <- numMeasurementsTable$numMeasRecords[1]
      numMeasRecordsWithValues <- numMeasurementsTable$numMeasRecordsWithValues[1]

      if (numMeasRecordsWithValues == 0 || numMeasRecords == 0) {
        propMeasRecordsWithValues <- 0
      } else {
        propMeasRecordsWithValues <- numMeasRecordsWithValues / numMeasRecords
      }

      measRecordsWithValues <- as.data.frame(cbind("propMeasRecordsWithValues", numMeasRecordsWithValues, propMeasRecordsWithValues)) %>%
        rename(
          "statistic" = "V1",
          "value" = "numMeasRecordsWithValues",
          "proportion" = "propMeasRecordsWithValues"
        ) %>%
        mutate(
          spec = case_when(
            desiredObservation == 1 ~ "Measurements with values desired",
            desiredObservation == 0 ~ "Measurements with values not desired"
          ),
          evaluateThreshold = desiredMeasurementValues
        )

      finalOutput <- rbind(personsWithCalendarTime, measRecordsWithValues)

      # Evaluate diagnostics for recommended Dbs per study question -----------

      personOutputSum <- personOutput %>%
        group_by(statistic, spec, evaluateThreshold) %>%
        summarise(value = sum(countValue)) %>%
        mutate(proportion = value / numPersonsInDb)

      personOutputSum <- rbind(finalOutput, personOutputSum)

      personOutputSum <- personOutputSum %>%
        left_join(ddThresholds,
          by = c("statistic" = "statistic")
        )

      # Evaluate results against thresholds

      sampleSizeValues <- personOutputSum %>%
        filter(evaluateThreshold == 1) %>%
        select("proportion")

      if (requiredIP == 1) {
        ipProp <- personOutputSum %>%
          filter(statistic == "propWithIPCriteria") %>%
          select("proportion")

        sampleSizeValues <- rbind(sampleSizeValues, ipProp)
      }

      if (requiredER == 1) {
        erProp <- personOutputSum %>%
          filter(statistic == "propWithERCriteria") %>%
          select("proportion")

        sampleSizeValues <- rbind(sampleSizeValues, erProp)
      }

      if (requiredOP == 1) {
        opProp <- personOutputSum %>%
          filter(statistic == "propWithOPCriteria") %>%
          select("proportion")

        sampleSizeValues <- rbind(sampleSizeValues, opProp)
      }

      dataDiagnosticsOutput <- personOutputSum %>%
        mutate(
          status = case_when(
            proportion <= threshold ~ "fail",
            proportion > threshold ~ "pass"
          ),
          fail = case_when(
            proportion <= threshold ~ 1,
            proportion > threshold ~ 0
          )
        )

      if (studySpecs$includeIndicationInCalc) {
        minSampleSizeProp <- min(as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredTargetConcepts"), ]$proportion),
          as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredComparatorConcepts"), ]$proportion),
          na.rm = TRUE
        ) * prod(as.numeric(sampleSizeValues[, 1])) * as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredIndicationConcepts"), ]$proportion)
      } else {
        minSampleSizeProp <- min(as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredTargetConcepts"), ]$proportion),
          as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredComparatorConcepts"), ]$proportion),
          na.rm = TRUE
        ) * prod(as.numeric(sampleSizeValues[, 1]))
      }

      minSampleSize <- round(minSampleSizeProp * numPersonsInDb, digits = 0)

      minSample <- list(
        statistic = "minSampleSize",
        value = minSampleSize,
        proportion = minSampleSizeProp,
        spec = "> 1000",
        evaluateThreshold = 1,
        threshold = 0,
        status = "pass",
        fail = 0
      )

      if (studySpecs$includeIndicationInCalc) {
        maxSampleSizeProp <- min(as.numeric(sampleSizeValues[, 1]),
          as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredTargetConcepts"), ]$proportion),
          as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredComparatorConcepts"), ]$proportion),
          as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredIndicationConcepts"), ]$proportion),
          na.rm = TRUE
        )
      } else {
        maxSampleSizeProp <- min(as.numeric(sampleSizeValues[, 1]),
          as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredTargetConcepts"), ]$proportion),
          as.numeric(personOutputSum[which(personOutputSum$statistic == "propWithRequiredComparatorConcepts"), ]$proportion),
          na.rm = TRUE
        )
      }

      maxSampleSize <- maxSampleSizeProp * numPersonsInDb

      if (maxSampleSize < 1000) {
        maxSampleStatus <- "fail"
        maxSampleFail <- 1
      } else {
        maxSampleStatus <- "pass"
        maxSampleFail <- 0
      }

      maxSample <- list(
        statistic = "maxSampleSize",
        value = maxSampleSize,
        proportion = maxSampleSizeProp,
        spec = "> 1000",
        evaluateThreshold = 1,
        threshold = 1000,
        status = maxSampleStatus,
        fail = maxSampleFail
      )

      dataDiagnosticsOutput <- rbind(dataDiagnosticsOutput, minSample, maxSample)

      dataDiagnosticsOutput <- dataDiagnosticsOutput %>%
        filter(evaluateThreshold > 0) %>%
        mutate(
          analysisId = analysisId,
          analysisName = analysisName,
          databaseId = dbName, .before = statistic
        )

      if (k == 1) {
        dataDiagnosticsResults <- dataDiagnosticsOutput
      } else {
        dataDiagnosticsResultsNew <- rbind(dataDiagnosticsResults, dataDiagnosticsOutput)
        dataDiagnosticsResults <- dataDiagnosticsResultsNew
      }
    } # end of for loop around analysis list

    CohortGenerator::writeCsv(dataDiagnosticsResults, file.path(outputFolder, "data_diagnostics_output.csv"), append = (i != 1))

    if (i == 1) {
      totalResults <- dataDiagnosticsResults
    } else {
      totalResultsNew <- rbind(dataDiagnosticsResults, totalResults)
      totalResults <- totalResultsNew
    }
  } # end of for loop around database list

  dbDiagnosticsSummary <- DbDiagnostics::createDataDiagnosticsSummary(totalResults)
  CohortGenerator::writeCsv(dbDiagnosticsSummary, file.path(outputFolder, "data_diagnostics_summary.csv"))

  tempFileName <- tempfile()

  ddAnalysisToRow <- function(ddAnalysis) {
    ParallelLogger::saveSettingsToJson(ddAnalysis, tempFileName)
    row <- tibble(
      analysisId = ddAnalysis$analysisId,
      description = ddAnalysis$description,
      definition = readChar(tempFileName, file.info(tempFileName)$size)
    )
    invisible(row)
  }

  dataDiagnosticsAnalysis <- lapply(dataDiagnosticsSettingsList, ddAnalysisToRow)
  dataDiagnosticsAnalysis <- bind_rows(dataDiagnosticsAnalysis) %>%
    distinct()

  unlink(tempFileName)

  fileName <- file.path(outputFolder, "data_diagnostics_analysis.csv")
  CohortGenerator::writeCsv(dataDiagnosticsAnalysis, fileName)

  invisible(totalResults)
}
