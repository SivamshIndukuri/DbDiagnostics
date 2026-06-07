SELECT
  COALESCE(SUM(CASE WHEN analysis_id = 1801 THEN count_value END), 0) AS num_meas_records,
  COALESCE(MAX(CASE WHEN analysis_id = 1814 THEN count_value END), 0) AS num_meas_records_with_values
FROM @results_database_schema.@results_table_name
WHERE analysis_id IN (1801, 1814)
  AND release_key = '@databaseName'