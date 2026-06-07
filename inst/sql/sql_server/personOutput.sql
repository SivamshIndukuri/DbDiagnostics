-- Age: birth year range (stratum_1 is varchar, cast to INT for numeric comparison)
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

-- Age at first observation
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

-- Gender (cast stratum_1 to INT to match integer concept ID list)
SELECT
  count_value,
  'propWithGenderCriteria' AS statistic,
  '@genderConceptIds' AS spec,
  1 AS evaluate_threshold
FROM @results_database_schema.@results_table_name
WHERE analysis_id = 2
  AND release_key = '@databaseName'
  AND CAST(stratum_1 AS INT) IN (@genderConceptIds)

UNION ALL

-- Race
SELECT
  count_value,
  'propWithRaceCriteria' AS statistic,
  '@raceConceptIds' AS spec,
  1 AS evaluate_threshold
FROM @results_database_schema.@results_table_name
WHERE analysis_id = 4
  AND release_key = '@databaseName'
  AND CAST(stratum_1 AS INT) IN (@raceConceptIds)

UNION ALL

-- Ethnicity
SELECT
  count_value,
  'propWithEthnicityCriteria' AS statistic,
  '@ethnicityConceptIds' AS spec,
  1 AS evaluate_threshold
FROM @results_database_schema.@results_table_name
WHERE analysis_id = 5
  AND release_key = '@databaseName'
  AND CAST(stratum_1 AS INT) IN (@ethnicityConceptIds)

UNION ALL

-- Longitudinality (stratum_1 is months as varchar, cast to NUMERIC)
SELECT
  count_value,
  'propWithLongitudinalCriteria' AS statistic,
  '@requiredDurationDays days' AS spec,
  1 AS evaluate_threshold
FROM @results_database_schema.@results_table_name
WHERE analysis_id = 108
  AND release_key = '@databaseName'
  AND CAST(stratum_1 AS NUMERIC) >= ROUND(@requiredDurationDays / 30.0, 0)

UNION ALL

-- Data Domain Coverage: required bit string (stratum_1 is already varchar)
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
  COALESCE(MAX(count_value), 0) AS count_value,
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
  COALESCE(MAX(count_value), 0) AS count_value,
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

-- Inpatient visits
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

-- Outpatient visits
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

-- Emergency Room visits
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

-- Target concepts (cast stratum_1 to INT)
SELECT
  COALESCE(MAX(count_value), 0) AS count_value,
  'propWithRequiredTargetConcepts' AS statistic,
  '@target' AS spec,
  2 AS evaluate_threshold
FROM @results_database_schema.@results_table_name
WHERE analysis_id IN (1800, 400, 600, 700, 800, 2100)
  AND release_key = '@databaseName'
  AND CAST(stratum_1 AS INT) IN (@requiredTargetConcepts)

UNION ALL

-- Comparator concepts (optional)
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
    AND CAST(stratum_1 AS INT) IN (@requiredComparatorConcepts)
}

UNION ALL

-- Indication concepts (optional)
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
    AND CAST(stratum_1 AS INT) IN (@requiredIndicationConcepts)
}

UNION ALL

-- Outcome concepts (optional)
{@requiredOutcomeConcepts == ''} ? {
  SELECT
    -1 AS count_value,
    'propWithRequiredOutcomeConcepts' AS statistic,
    'NA' AS spec,
    0 AS evaluate_threshold
} : {
  SELECT
    COALESCE(MAX(count_value), 0) AS count_value,
    'propWithRequiredOutcomeConcepts' AS statistic,
    '@outcome' AS spec,
    2 AS evaluate_threshold
  FROM @results_database_schema.@results_table_name
  WHERE analysis_id IN (1800, 400, 600, 700, 800, 2100)
    AND release_key = '@databaseName'
    AND CAST(stratum_1 AS INT) IN (@requiredOutcomeConcepts)
}