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