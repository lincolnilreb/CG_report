WITH surveys AS (
  SELECT
    client_id,
    "1806" AS "q_1806_budget_rank",
    "1801" AS "q_1806_test",
    "644" AS "q_1806_history",
    "667" AS "q_1806_history",
    "1805" AS "q_1805_prefer_nut_pattern"
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
        AND surveys.question_id IN (1806,1805,1801,644,667)
    )
    SELECT client_id, question_id, string_agg(answer, ' || ')
    FROM surveys
    GROUP BY client_id, question_id
  $$
  ) AS ct(
    "client_id" INT,
    "1801" TEXT,
    "1805" TEXT,
    "644" TEXT,
    "667" TEXT,
    "1806" TEXT
  )
)

SELECT
  clients.id, clients.name, clients.gender, clients.birthday AS btd, clients.mobile, programs.name AS program_name, orgs.name AS org_name, group_classes.started_at AS date_T0, group_classes.finished_at AS date_T1,
  surveys.*
FROM clients
INNER JOIN group_class_orders
ON group_class_orders.client_id = clients.id
INNER JOIN group_classes
ON group_classes.id = group_class_orders.group_class_id
INNER JOIN programs
ON programs.id = group_classes.program_id
LEFT JOIN orgs
ON orgs.id = group_classes.org_id
LEFT JOIN surveys
ON surveys.client_id = clients.id
WHERE programs.org_id IN (3, 21, 24, 26, 29, 30, 33, 34)
