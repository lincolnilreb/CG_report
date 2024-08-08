CREATE MATERIALIZED VIEW "10_daily_flc" AS (

WITH
  group_classes_of_flc_program_join_users AS (
    SELECT
      group_classes.name AS class_name,
      programs.name AS program_name,
      CONCAT(users.first_name, users.last_name) AS user_name,
      group_classes.id AS group_class_id,
      group_classes.started_at,
      group_classes.finished_at
    FROM group_classes
    INNER JOIN programs ON group_classes.program_id = programs.id
    INNER JOIN users ON group_classes.user_id = users.id
    WHERE programs.org_id = 3
      AND group_classes.id NOT IN (8, 24, 28, 34)
--       AND group_classes.name NOT LIKE '%初日%' AND group_classes.name NOT LIKE '%宋醫師進階%' AND group_classes.name NOT LIKE '%診所進階%'
--    Temp
--        AND group_classes.created_at > '2023-04-01'
  ),
  group_classes_of_flc_program_join_users_and_clients AS (
    SELECT DISTINCT ON (clients.id, group_class_orders.group_class_id)
      group_classes_of_flc_program_join_users.*,
      group_class_orders.client_id,
      group_class_orders.group_class_id AS class_id,
      clients.birthday,
      clients.gender,
      clients.current,
      clients.mobile
    FROM group_class_orders
    INNER JOIN clients ON group_class_orders.client_id = clients.id
    INNER JOIN group_classes_of_flc_program_join_users ON group_class_orders.group_class_id = group_classes_of_flc_program_join_users.group_class_id
    --    Temp
  ),
  notes_of_flc_courses AS (
    SELECT
      notes.*,
      SUM((notes.data->>'carbohydrate')::NUMERIC) AS carbohydrate,
      SUM((notes.data->>'protein')::NUMERIC) AS protein,
      SUM((notes.data->>'fat')::NUMERIC) AS fat,
      COUNT(notes.client_id) AS note_assets_count,
      COUNT(notes.client_id) FILTER (WHERE note_assets.data->>'light' = 'green') AS note_assets_g_light_count,
      COUNT(notes.client_id) FILTER (WHERE note_assets.data->>'light' = 'yellow') AS note_assets_y_light_count,
      COUNT(notes.client_id) FILTER (WHERE note_assets.data->>'light' = 'red')  AS note_assets_r_light_count,
      gcflcuc.class_id,
      gcflcuc.class_name,
      gcflcuc.program_name,
      gcflcuc.user_name,
      gcflcuc.started_at,
      gcflcuc.finished_at,
      gcflcuc.birthday,
      gcflcuc.gender,
      gcflcuc.current,
      gcflcuc.mobile,
      ccs.weight,
      ccs.bmi,
      ccs.waist_circumference,
      ccs.body_fat_mass
    FROM notes
    INNER JOIN group_classes_of_flc_program_join_users_and_clients gcflcuc
      ON gcflcuc.client_id = notes.client_id
      AND notes.date BETWEEN gcflcuc.started_at AND gcflcuc.finished_at
    LEFT JOIN note_assets
      ON note_assets.note_id = notes.id
    LEFT JOIN consulting_client_summaries ccs
      ON ccs.client_id = notes.client_id
      AND ccs.date = notes.date
    WHERE note_assets.url IS NOT NULL
    GROUP BY notes.id, notes.client_id, notes.date,
              gcflcuc.birthday,
              gcflcuc.gender,
              gcflcuc.current,
              gcflcuc.mobile,
              gcflcuc.class_id,
              gcflcuc.class_name,
              gcflcuc.program_name,
              gcflcuc.user_name,
              gcflcuc.started_at,
              gcflcuc.finished_at,
              ccs.weight, ccs.bmi, ccs.waist_circumference, ccs.body_fat_mass
    --    Temp
--        WHERE notes.created_at BETWEEN '2023-04-01' AND '2023-05-01'
  )
SELECT
  notes_of_flc_courses.class_id,
  notes_of_flc_courses.class_name AS class_name,
  notes_of_flc_courses.program_name AS program,
  notes_of_flc_courses.started_at AS date_flc_T0,
  notes_of_flc_courses.finished_at AS date_flc_T1,
  notes_of_flc_courses.user_name AS nutritionist_online,
  notes_of_flc_courses.client_id,
  notes_of_flc_courses.mobile,
  notes_of_flc_courses.date AS date,
  notes_of_flc_courses.birthday AS btd,
  AGE(CURRENT_DATE, notes_of_flc_courses.birthday),
  notes_of_flc_courses.gender,
  notes_of_flc_courses.current->>'height' AS height,
  SUM(notes_of_flc_courses.note_assets_count) AS pic_count,
  SUM(notes_of_flc_courses.note_assets_g_light_count) AS light_G_count,
  SUM(notes_of_flc_courses.note_assets_y_light_count) AS light_Y_count,
  SUM(notes_of_flc_courses.note_assets_r_light_count) AS light_R_count,
  (SUM(COALESCE(notes_of_flc_courses.note_assets_g_light_count, 0))) / NULLIF(SUM(COALESCE(notes_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_G_p,
  (SUM(COALESCE(notes_of_flc_courses.note_assets_y_light_count, 0))) / NULLIF(SUM(COALESCE(notes_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_Y_p,
  (SUM(COALESCE(notes_of_flc_courses.note_assets_r_light_count, 0))) / NULLIF(SUM(COALESCE(notes_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_R_p,
  SUM(notes_of_flc_courses.carbohydrate) AS carbohydrate,
  SUM(notes_of_flc_courses.protein) AS protein,
  SUM(notes_of_flc_courses.fat) AS fat,
  weight,
  bmi,
  waist_circumference,
  body_fat_mass
FROM notes_of_flc_courses
GROUP BY
  notes_of_flc_courses.class_id,
  notes_of_flc_courses.class_name,
  notes_of_flc_courses.program_name,
  notes_of_flc_courses.started_at,
  notes_of_flc_courses.finished_at,
  notes_of_flc_courses.user_name,
  notes_of_flc_courses.client_id,
  notes_of_flc_courses.mobile,
  notes_of_flc_courses.birthday,
  notes_of_flc_courses.gender,
  notes_of_flc_courses.current->>'height',
  notes_of_flc_courses.date,
  weight, bmi, waist_circumference, body_fat_mass
)
