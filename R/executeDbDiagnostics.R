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
#' @param connectionDetails             A connectionDetails object for connecting to the database containing the DbProfile results
#' @param resultsDatabaseSchema         The fully qualified database name of the results schema where the DbProfile results are housed. Default is "dp_temp".
#' @param resultsTableName              The name of the table in the results schema with the DbProfile results. Default is "dp_achilles_results_augmented."
#' @param outputFolder                  Results will be written to this directory, default = getwd()
#' @param dataDiagnosticsSettingsList   A list of settings objects, each created from DataDiagnostics::createDataDiagnosticsSettings() function and each representing one analysis.
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
  conceptIdsTable <- DatabaseConnector::querySql(conn, tsql, snakeCaseToCamelCase = TRUE)

  dbNum <- nrow(dbNames)

  for (i in 1:dbNum) {
    dbName <- dbNames[i, 2]
    message(paste0("Database: ", dbName, " (", i, "/", nrow(dbNames), ")"))
    dbProfile <- conceptIdsTable %>% filter(releaseKey == dbName)

    for (k in 1:length(dataDiagnosticsSettingsList)) {
      studySpecs <- dataDiagnosticsSettingsList[[k]]

      # TODO ---------------
      # evaluate the specs input
      # look at data types and stop if target concept id is null
      if (is.null(studySpecs$targetConceptIds)) {
        stop("Need to specify targetConceptIds")
      }
      checkmate::assertIntegerish(studySpecs$analysisId, null.ok = FALSE, len = 1)
      checkmate::assertString(studySpecs$analysisName, null.ok = FALSE, min.chars = 1)
      checkmate::assertNumeric(studySpecs$minAge, null.ok = TRUE)
      checkmate::assertNumeric(studySpecs$maxAge, null.ok = TRUE)
      checkmate::assertIntegerish(studySpecs$genderConceptIds, null.ok = FALSE)
      checkmate::assertIntegerish(studySpecs$raceConceptIds, null.ok = TRUE)
      checkmate::assertIntegerish(studySpecs$ethnicityConceptIds, null.ok = TRUE)
      checkmate::assertString(studySpecs$studyStartDate, null.ok = FALSE, min.chars = 6, max.chars = 6)
      checkmate::assertString(studySpecs$studyEndDate, null.ok = FALSE, min.chars = 6, max.chars = 6)
      checkmate::assertIntegerish(studySpecs$requiredDurationDays, null.ok = FALSE)
      
      allowed_visits <- c("IP", "OP", "ER")
      if (!is.null(studySpecs$requiredVisits)) {
      	checkmate::assertSubset(studySpecs$requiredVisits, choices = allowed_visits, empty.ok = FALSE)
      }

      if (!is.null(studySpecs$desiredVisits)) {
      	checkmate::assertSubset(studySpecs$desiredVisits, choices = allowed_visits, empty.ok = FALSE)
      }
      
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
        minAge <- min(as.integer(dbProfile[which(dbProfile$analysisId == 101), ]$stratum1))
      } else {
        minAge <- studySpecs$minAge
        numCriteria <- numCriteria + 1
      }
      if (is.null(studySpecs$maxAge)) {
        maxAge <- max(as.integer(dbProfile[which(dbProfile$analysisId == 101), ]$stratum1))
      } else {
        maxAge <- studySpecs$maxAge
        numCriteria <- numCriteria + 1
      }

      maxYearInDb <- as.integer(substr(max(dbProfile[which(dbProfile$analysisId == 111), ]$stratum1), 1, 4))
      minYearInDb <- as.integer(substr(min(dbProfile[which(dbProfile$analysisId == 111), ]$stratum1), 1, 4))
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
          filter(analysisId == 4) %>%
          select(stratum1) %>%
          .[["stratum1"]]
      } else {
        raceConceptIds <- studySpecs$raceConceptIds
        numCriteria <- numCriteria + 1
      }

      # Ethnicity
      if (is.null(studySpecs$ethnicityConceptIds)) {
        ethnicityConceptIds <- dbProfile %>%
          filter(analysisId == 5) %>%
          select(stratum1) %>%
          .[["stratum1"]]
      } else {
        ethnicityConceptIds <- studySpecs$ethnicityConceptIds
        numCriteria <- numCriteria + 1
      }

      # Study Start Date
      if (is.null(studySpecs$studyStartDate)) {
        studyStartDate <- min(dbProfile[which(dbProfile$analysisId == 111), ]$stratum1)
      } else {
        studyStartDate <- max(as.numeric(studySpecs$studyStartDate), min(dbProfile[which(dbProfile$analysisId == 111), ]$stratum1))
        numCriteria <- numCriteria + 1
      }

      # Study End Date
      if (is.null(studySpecs$studyEndDate)) {
        studyEndDate <- max(dbProfile[which(dbProfile$analysisId == 112), ]$stratum1)
      } else {
        studyEndDate <- min(as.numeric(studySpecs$studyEndDate), max(dbProfile[which(dbProfile$analysisId == 111), ]$stratum1))
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

      tsql <- SqlRender::loadRenderTranslateSql(
        sqlFilename = "personOutput.sql",
        packageName = "DbDiagnostics",
        dbms = connectionDetails$dbms,
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
      personOutput <- DatabaseConnector::querySql(conn, tsql, snakeCaseToCamelCase = TRUE)

      personOutput$countValue[personOutput$countValue == -1] <- NA
      personOutput$spec[personOutput$spec == "NA"] <- NA

      # Calendar Time ----------
      calendarStats <- DatabaseConnector::renderTranslateQuerySql(
        connection = conn,
        sql = SqlRender::loadRenderTranslateSql(
          sqlFilename = "calendarStats.sql",
          packageName = "DbDiagnostics",
          dbms = connectionDetails$dbms,
          results_database_schema = resultsDatabaseSchema,
          results_table_name = resultsTableName,
          databaseName = dbName,
          studyStartDate = studyStartDate,
          studyEndDate = studyEndDate
        ),
        snakeCaseToCamelCase = TRUE
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
      numMeasurementsTable <- DatabaseConnector::renderTranslateQuerySql(
        connection = conn,
        sql = SqlRender::loadRenderTranslateSql(
          sqlFilename = "measurementStats.sql",
          packageName = "DbDiagnostics",
          dbms = connectionDetails$dbms,
          results_database_schema = resultsDatabaseSchema,
          results_table_name = resultsTableName,
          databaseName = dbName
        ),
        snakeCaseToCamelCase = TRUE
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
            desiredMeasurementValues == 1 ~ "Measurements with values desired",
            desiredMeasurementValues == 0 ~ "Measurements with values not desired"
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
