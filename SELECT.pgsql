SELECT current_database();
SELECT
    c.column_name,
    c.data_type,
    c.udt_name,
    c.is_nullable,
    EXISTS (
        SELECT 1
        FROM information_schema.key_column_usage k
        JOIN information_schema.table_constraints tc
          ON tc.constraint_schema = k.constraint_schema
         AND tc.constraint_name = k.constraint_name
         AND tc.table_name = k.table_name
        WHERE k.table_schema = c.table_schema
          AND k.table_name = c.table_name
          AND k.column_name = c.column_name
          AND tc.constraint_type IN ('PRIMARY KEY', 'UNIQUE')
    ) AS is_unique_key
FROM information_schema.columns c
WHERE c.table_schema = 'public'
  AND c.table_name = 'obs_cat'
ORDER BY c.ordinal_position;