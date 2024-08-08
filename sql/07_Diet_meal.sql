-- 07 Diet meal

SELECT
  client_id, date AS date_diet, data->>'meal_order' AS meal_order,
  clients.mobile,  
  SUM((data->>'carbohydrate')::NUMERIC) AS carbohydrate,
  SUM((data->>'protein')::NUMERIC) AS protein,
  SUM((data->>'fat')::NUMERIC) AS fat,
  SUM((data->>'calorie')::NUMERIC) AS calorie,
  SUM((data->>'fruits')::NUMERIC) AS fruits,
  SUM((data->>'vegetables')::NUMERIC) AS vegetables,
  SUM((data->>'grains')::NUMERIC) AS grains,
  SUM((data->>'meat_beans_low_fat')::NUMERIC) AS meat_beans_low_fat,
  SUM((data->>'meat_beans_medium_fat')::NUMERIC) AS meat_beans_medium_fat,
  SUM((data->>'meat_beans_high_fat')::NUMERIC) AS meat_beans_high_fat,
  SUM((data->>'milk_whole_fat')::NUMERIC) AS milk_whole_fat,
  SUM((data->>'milk_low_fat')::NUMERIC) AS milk_low_fat,
  SUM((data->>'milk_skim')::NUMERIC) AS milk_skim,
  SUM((data->>'oil')::NUMERIC) AS oil
FROM notes
INNER JOIN clients ON clients.id = notes.client_id
WHERE type = 'FoodNote'
--  AND date > '2023-2-20'
GROUP BY client_id, date, data->>'meal_order', clients.mobile
