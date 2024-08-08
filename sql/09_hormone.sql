-- 09
-- COUNT choice.score group by questions where question_set_ids IN question_set.ids
-- 
--EXPLAIN ANALYZE
SELECT
  *
FROM CROSSTAB(
  $$
  WITH surveys_with_answers AS (
    SELECT
      surveys.target_id AS client_id,
      clients.mobile,
      surveys.taken_at AS date_hormone,
      survey_answers.*
    FROM surveys
    INNER JOIN clients ON clients.id = surveys.target_id
    INNER JOIN question_sets
      ON question_sets.survey_form_id = surveys.survey_form_id
    INNER JOIN (
      SELECT survey_id, answer, answers.choice_id::INTEGER
      FROM survey_answers,
      JSONB_TO_RECORDSET(answer) AS answers(choice_id TEXT)
    ) AS survey_answers
      ON survey_answers.survey_id = surveys.id
    WHERE surveys.survey_form_id IN (
      SELECT id
      FROM survey_forms
      WHERE data->>'scores_name' = 'hormone_scores'
    )
      AND question_sets.score_calculation_type IN (4)
      AND surveys.target_type = 'Client'
      AND JSONB_TYPEOF(survey_answers.answer) = 'array'
    ),
    LP_scores_of_choices AS (
      SELECT choices.id, UPPER(choices.score) AS score FROM choices
      INNER JOIN questions ON questions.id = choices.question_id
      INNER JOIN question_sets on question_sets.id = questions.question_set_id
      WHERE question_sets.score_calculation_type = 3
    ),
    tclea_scores_of_choices AS (
      SELECT choices.id, LOWER(choices.score) AS score FROM choices
      INNER JOIN questions ON questions.id = choices.question_id
      INNER JOIN question_sets on question_sets.id = questions.question_set_id
      WHERE question_sets.score_calculation_type = 4
    )
  SELECT client_id, mobile, surveys_with_answers.date_hormone, choices.score, COUNT(choices.score)
  FROM surveys_with_answers
  INNER JOIN LP_scores_of_choices AS choices
  ON choices.id = surveys_with_answers.choice_id
  GROUP BY client_id, mobile, date_hormone, choices.score
  UNION
  SELECT surveys_with_answers.client_id, mobile, surveys_with_answers.date_hormone, choices.score, COUNT(choices.score)
  FROM surveys_with_answers
  INNER JOIN tclea_scores_of_choices AS choices
  ON choices.id = surveys_with_answers.choice_id
  GROUP BY client_id, mobile, date_hormone, choices.score
  $$, $$VALUES('L'), ('P'), ('t'), ('c'), ('l'), ('e'), ('a')$$
) AS ct("client_id" INT, "mobile" TEXT, "date_hormone" TEXT, "hormone_L" TEXT, "hormone_P" TEXT, "hormone_t" INT, "hormone_c" TEXT, "hormone_l" TEXT, "hormone_e" INT, "hormone_a" INT)
