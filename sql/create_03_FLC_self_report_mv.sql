SELECT * FROM "03_FLC_self_report";

--DROP MATERIALIZED VIEW "03_FLC_self_report";

CREATE MATERIALIZED VIEW "03_FLC_self_report" AS (
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
      --  AND group_classes.created_at > '2023-01-01'
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
      SELECT notes.*, group_classes_of_flc_program_join_users_and_clients.class_id
      FROM notes
      INNER JOIN group_classes_of_flc_program_join_users_and_clients
        ON group_classes_of_flc_program_join_users_and_clients.client_id = notes.client_id
        AND notes.date BETWEEN group_classes_of_flc_program_join_users_and_clients.started_at AND group_classes_of_flc_program_join_users_and_clients.finished_at
      --    Temp
  --      WHERE notes.created_at BETWEEN '2023-03-01' AND '2023-04-01'
    ),
    note_assets_of_flc_courses AS (
      SELECT
        note_assets.id, note_assets.data, notes_of_flc_courses.client_id,
        note_assets.data->>'light' AS light,
        note_assets.date,
        note_assets.note_id
      FROM note_assets
      INNER JOIN notes_of_flc_courses
      ON notes_of_flc_courses.id = note_assets.note_id
      WHERE note_assets.url IS NOT NULL
    ),
    notes_aggregation_of_flc_courses AS (
      SELECT
--      DISTINCT ON (notes.client_id, notes.date, notes.class_id)
        notes.client_id,
        notes.class_id,
        SUM((notes.data->>'carbohydrate')::NUMERIC) AS carbohydrate,
        SUM((notes.data->>'protein')::NUMERIC) AS protein,
        SUM((notes.data->>'fat')::NUMERIC) AS fat,
--        COUNT(notes.date) OVER(PARTITION BY notes.client_id, notes.date) AS notes_count,
        notes.date,
        COUNT(notes.client_id) AS note_assets_count,
        COUNT(notes.client_id) FILTER (WHERE light = 'green') AS note_assets_g_light_count,
        COUNT(notes.client_id) FILTER (WHERE light = 'yellow') AS note_assets_y_light_count,
        COUNT(notes.client_id) FILTER (WHERE light = 'red')  AS note_assets_r_light_count
      FROM notes_of_flc_courses AS notes
      LEFT JOIN note_assets_of_flc_courses AS note_assets ON note_assets.note_id = notes.id
      GROUP BY notes.client_id, notes.date, notes.class_id
    ),
    consulting_client_summaries_of_flc_courses AS (
      SELECT ccs.*, group_classes_of_flc_program_join_users_and_clients.class_id
      FROM consulting_client_summaries ccs
      INNER JOIN group_classes_of_flc_program_join_users_and_clients
      ON group_classes_of_flc_program_join_users_and_clients.client_id = ccs.client_id
      AND ccs.date BETWEEN group_classes_of_flc_program_join_users_and_clients.started_at - 7 AND group_classes_of_flc_program_join_users_and_clients.finished_at + 7
    ),
    consulting_client_summaries_before_flc_courses AS (
      SELECT DISTINCT ON (ccsflc.client_id, ccsflc.class_id) ccsflc.*
      FROM  consulting_client_summaries_of_flc_courses AS ccsflc
      ORDER BY ccsflc.client_id, ccsflc.class_id, ccsflc.date ASC
    ),
    consulting_client_summaries_after_flc_courses AS (
      SELECT DISTINCT ON (ccsflc.client_id, ccsflc.class_id) ccsflc.*
      FROM  consulting_client_summaries_of_flc_courses AS ccsflc
      ORDER BY ccsflc.client_id, ccsflc.class_id, ccsflc.date DESC
    )
  
  SELECT
    group_classes_of_flc_program_join_users_and_clients.class_id,
    group_classes_of_flc_program_join_users_and_clients.class_name AS class_name,
    group_classes_of_flc_program_join_users_and_clients.program_name AS program,
    group_classes_of_flc_program_join_users_and_clients.started_at AS date_flc_T0,
    group_classes_of_flc_program_join_users_and_clients.finished_at AS date_flc_T1,
    group_classes_of_flc_program_join_users_and_clients.user_name AS nutritionist_online,
    group_classes_of_flc_program_join_users_and_clients.client_id,
    group_classes_of_flc_program_join_users_and_clients.mobile,
    MAX(notes_aggregation_of_flc_courses.date) AS date_latest_update,
--    MAX(notes_aggregation_of_flc_courses.date) OVER (PARTITION BY group_classes_of_flc_program_join_users_and_clients.client_id, group_classes_of_flc_program_join_users_and_clients.class_id) AS date_latest_update,
    group_classes_of_flc_program_join_users_and_clients.birthday AS btd,
    AGE(CURRENT_DATE, group_classes_of_flc_program_join_users_and_clients.birthday),
  --  (CURRENT_DATE - group_classes_of_flc_program_join_users_and_clients.birthday) / 365 AS age,
    group_classes_of_flc_program_join_users_and_clients.gender,
    group_classes_of_flc_program_join_users_and_clients.current->>'height' AS height,
    COUNT(notes_aggregation_of_flc_courses.date) AS day_count,
    SUM(notes_aggregation_of_flc_courses.note_assets_count) AS pic_count,
    SUM(notes_aggregation_of_flc_courses.note_assets_g_light_count) AS light_G_count,
    (SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0))) / NULLIF(SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_G_p,
    (SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0))) / NULLIF(SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_Y_p,
    (SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0))) / NULLIF(SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_g_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_y_light_count, 0)) + SUM(COALESCE(notes_aggregation_of_flc_courses.note_assets_r_light_count, 0)), 0) AS light_R_p,
    SUM(notes_aggregation_of_flc_courses.carbohydrate) AS carbohydrate,
    SUM(notes_aggregation_of_flc_courses.protein) AS protein,
    SUM(notes_aggregation_of_flc_courses.fat) AS fat,
    consulting_client_summaries_before.date AS measurement_before_program_date,
    consulting_client_summaries_after.date AS measurement_after_program_date,
    consulting_client_summaries_before.weight AS weight_before,
    consulting_client_summaries_after.weight AS weight_after,
    consulting_client_summaries_after.weight - consulting_client_summaries_before.weight AS weight_delta,
    (consulting_client_summaries_after.weight - consulting_client_summaries_before.weight) / NULLIF(consulting_client_summaries_before.weight, 0) AS weight_delta_p,
    consulting_client_summaries_before.bmi AS bmi_before,
    consulting_client_summaries_after.bmi AS bmi_after,
    consulting_client_summaries_after.bmi - consulting_client_summaries_before.bmi AS bmi_delta,
    (consulting_client_summaries_after.bmi - consulting_client_summaries_before.bmi) / NULLIF(consulting_client_summaries_before.bmi, 0) AS bmi_delta_p,
    consulting_client_summaries_before.body_fat_mass AS fat_before,
    consulting_client_summaries_after.body_fat_mass AS fat_after,
    consulting_client_summaries_after.body_fat_mass - consulting_client_summaries_before.body_fat_mass AS fat_delta,
    (consulting_client_summaries_after.body_fat_mass - consulting_client_summaries_before.body_fat_mass) / NULLIF(consulting_client_summaries_before.body_fat_mass, 0) AS fat_delta_p,
    consulting_client_summaries_before.waist_circumference AS wc_before,
    consulting_client_summaries_after.waist_circumference AS wc_after,
    consulting_client_summaries_after.waist_circumference - consulting_client_summaries_before.waist_circumference AS wc_delta,
    (consulting_client_summaries_after.waist_circumference - consulting_client_summaries_before.waist_circumference) / NULLIF(consulting_client_summaries_before.waist_circumference, 0) AS wc_delta_p
  FROM group_classes_of_flc_program_join_users_and_clients
  LEFT JOIN notes_aggregation_of_flc_courses
    ON notes_aggregation_of_flc_courses.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
    AND  notes_aggregation_of_flc_courses.class_id = group_classes_of_flc_program_join_users_and_clients.class_id
    AND notes_aggregation_of_flc_courses.date BETWEEN group_classes_of_flc_program_join_users_and_clients.started_at AND group_classes_of_flc_program_join_users_and_clients.finished_at
  LEFT JOIN consulting_client_summaries_before_flc_courses AS consulting_client_summaries_before
    ON consulting_client_summaries_before.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
    AND consulting_client_summaries_before.class_id = group_classes_of_flc_program_join_users_and_clients.class_id
  LEFT JOIN consulting_client_summaries_after_flc_courses AS consulting_client_summaries_after
    ON consulting_client_summaries_after.client_id = group_classes_of_flc_program_join_users_and_clients.client_id
    AND consulting_client_summaries_after.class_id = group_classes_of_flc_program_join_users_and_clients.class_id
  GROUP BY
    group_classes_of_flc_program_join_users_and_clients.class_id,
    group_classes_of_flc_program_join_users_and_clients.class_name,
    group_classes_of_flc_program_join_users_and_clients.program_name,
    group_classes_of_flc_program_join_users_and_clients.started_at,
    group_classes_of_flc_program_join_users_and_clients.finished_at,
    group_classes_of_flc_program_join_users_and_clients.user_name,
    group_classes_of_flc_program_join_users_and_clients.client_id,
    group_classes_of_flc_program_join_users_and_clients.mobile,
    group_classes_of_flc_program_join_users_and_clients.birthday,
    group_classes_of_flc_program_join_users_and_clients.gender,
    group_classes_of_flc_program_join_users_and_clients.current->>'height',
    consulting_client_summaries_before.weight,
    consulting_client_summaries_after.weight,
    consulting_client_summaries_before.bmi,
    consulting_client_summaries_after.bmi,
    consulting_client_summaries_before.body_fat_mass,
    consulting_client_summaries_after.body_fat_mass,
    consulting_client_summaries_before.waist_circumference,
    consulting_client_summaries_after.waist_circumference,
    consulting_client_summaries_before.date,
    consulting_client_summaries_after.date
)
