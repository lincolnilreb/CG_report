WITH surveys AS (
  SELECT
    client_id,
    "1844" AS "q_1844_menopause",
    "1845" AS "q_1845_menopause_detail",
    "1848" AS "q_1848_1st_ob_age",
    "1849" AS "q_1849_parents_ob",
    "1850" AS "q_1850_family_history",
    "1853" AS "q_1853_thin_weight",
    "1854" AS "q_1854_fat_weight",
    "1855" AS "q_1855_occupation",
    "1856" AS "q_1856_work_type",
    "1857" AS "q_1857_work_rotate",
    "1860" AS "q_1860_sleep_duration",
    "1861" AS "q_1861_sleep_quality",
    "1862" AS "q_1862_fecal_freq",
    "1865" AS "q_1865_history",
    "1866" AS "q_1866_current_medication",
    "1877" AS "q_1877_current_supplements",
    "1875" AS "q_1875_diet_habbit",
    "1876" AS "q_1876_appetite_attr",
    "1880" AS "q_1880_exercise",
    "1879" AS "q_1879_exercise_freq",
    "1882" AS "q_1882_ob_cause_self_report",
    "1883" AS "q_1883_weight_loss_history",
    "1884" AS "q_1884_difficulty"
  FROM CROSSTAB(
  $$
    WITH surveys AS (
      SELECT
        surveys.target_id AS client_id,
        surveys.question_id,
    --    CONCAT(surveys.answer#>'{answer}')
    --    jsonb_array_elements((surveys.answer->'answer')::jsonb),
    --    #>'{answer}'
        jsonb_array_elements(surveys.answer)->>'answer' AS answer
        FROM (
          SELECT
            surveys.*,
            answers.answer,
            answers.question_id
          FROM surveys, jsonb_to_recordset((surveys.data->>'answers')::JSONB) AS answers(answer JSONB, question_id NUMERIC)
          WHERE surveys.data->>'answers' IS NOT NULL
        ) AS surveys
      INNER JOIN survey_forms
      ON survey_forms.id = surveys.survey_form_id
        AND survey_forms.provider_type = 'Org'
      INNER JOIN orgs
      ON orgs.id = survey_forms.provider_id
      WHERE surveys.target_type = 'Client'
        AND orgs.id IN (3, 21, 24, 26)
        AND surveys.answer IS NOT NULL
        AND surveys.question_id IN (1844, 1845, 1848, 1849, 1850, 1853, 1854, 1855, 1856, 1857, 1860, 1861, 1862, 1865, 1866, 1877, 1875, 1876, 1880, 1879, 1882, 1883, 1884)
    )
    SELECT client_id, question_id, string_agg(answer, ' || ')
    FROM surveys
    GROUP BY client_id, question_id
  $$
  ) AS ct(
    "client_id" INT,
    "1844" TEXT,
    "1845" TEXT,
    "1848" TEXT,
    "1849" TEXT,
    "1850" TEXT,
    "1853" TEXT,
    "1854" TEXT,
    "1855" TEXT,
    "1856" TEXT,
    "1857" TEXT,
    "1860" TEXT,
    "1861" TEXT,
    "1862" TEXT,
    "1865" TEXT,
    "1866" TEXT,
    "1877" TEXT,
    "1875" TEXT,
    "1876" TEXT,
    "1880" TEXT,
    "1879" TEXT,
    "1882" TEXT,
    "1883" TEXT,
    "1884" TEXT
  )
)

SELECT
  org_client_profiles.branch_id, clients.id, clients.name, org_client_profiles.identity_number, clients.gender, clients.birthday AS btd, group_classes.id AS class_id, users.real_name AS neutrionist_real_name, CONCAT(users.first_name, users.last_name) AS user_name, clients.mobile, programs.name AS program_name, orgs.name AS org_name, group_classes.started_at AS date_T0, group_classes.finished_at AS date_T1,
  surveys.*
FROM clients
INNER JOIN group_class_orders
ON group_class_orders.client_id = clients.id
INNER JOIN group_classes
ON group_classes.id = group_class_orders.group_class_id
INNER JOIN users
ON group_classes.user_id = users.id
INNER JOIN programs
ON programs.id = group_classes.program_id
LEFT JOIN orgs
ON orgs.id = group_classes.org_id
LEFT JOIN org_client_profiles
ON org_client_profiles.client_id = clients.id
LEFT JOIN surveys
ON surveys.client_id= clients.id
WHERE programs.org_id IN (3, 21, 24, 26, 29, 30, 33, 34)