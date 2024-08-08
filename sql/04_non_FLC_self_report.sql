-- 看起來 date 可能沒有處理 timezone 的問題
-- 沒有參加團班的 clients
SELECT
  consulting_client_summaries.client_id,
  clients.gender,
  clients.birthday,
  clients.mobile,
  (TIMEZONE('UTC', consulting_client_summaries.created_at) AT TIME ZONE 'Asia/Taipei')::DATE AS date_free_version,
  TIMEZONE('UTC', consulting_client_summaries.created_at) AT TIME ZONE 'Asia/Taipei' AS date_time,
  consulting_client_summaries.weight,
  consulting_client_summaries.bmi,
  consulting_client_summaries.body_fat_mass AS fat_mass,
  consulting_client_summaries.waist_circumference AS wc
FROM consulting_client_summaries
INNER JOIN clients ON clients.id = consulting_client_summaries.client_id
LEFT JOIN group_class_orders ON consulting_client_summaries.client_id = group_class_orders.client_id
WHERE group_class_orders.id IS NULL
  AND consulting_client_summaries.client_id IS NOT NULL
  AND (
    consulting_client_summaries.weight IS NOT NULL 
    OR consulting_client_summaries.bmi IS NOT NULL
    OR consulting_client_summaries.body_fat_mass IS NOT NULL
    OR consulting_client_summaries.waist_circumference IS NOT NULL
  )
-- INNER JOIN group_classes ON group_class_orders.group_class_id = group_classes.id
-- INNER JOIN programs ON group_classes.program_id = programs.id
-- WHERE programs.name NOT LIKE 'FLC'
ORDER BY client_id DESC, date DESC
