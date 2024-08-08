# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema.define(version: 2023_02_14_034955) do

  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"
  enable_extension "uuid-ossp"

  create_table "access_tokens", id: :serial, force: :cascade do |t|
    t.integer "user_id", null: false
    t.string "token_string", limit: 255, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "active_admin_comments", id: :serial, force: :cascade do |t|
    t.string "namespace"
    t.text "body"
    t.string "resource_id", null: false
    t.string "resource_type", null: false
    t.integer "author_id"
    t.string "author_type"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["author_id", "author_type"], name: "index_active_admin_comments_on_author_id_and_author_type"
    t.index ["author_type", "author_id"], name: "index_active_admin_comments_on_author_type_and_author_id"
    t.index ["namespace"], name: "index_active_admin_comments_on_namespace"
    t.index ["resource_id", "resource_type"], name: "index_active_admin_comments_on_resource_id_and_resource_type"
    t.index ["resource_type", "resource_id"], name: "index_active_admin_comments_on_resource_type_and_resource_id"
  end

  create_table "activity_diaries", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.date "date", null: false
    t.integer "calorie"
    t.jsonb "detail"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_activity_diaries_on_client_id"
  end

  create_table "activity_items", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "category"
    t.jsonb "detail"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "sub_category"
    t.decimal "numeric", precision: 4, scale: 2
    t.string "display_name"
    t.boolean "disabled", default: false
    t.integer "member_id"
    t.string "member_type"
    t.string "uid"
  end

  create_table "activity_records", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.integer "diary_id"
    t.integer "item_id"
    t.string "item_type"
    t.date "date", null: false
    t.integer "calorie", default: 0
    t.jsonb "detail"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_activity_records_on_client_id"
    t.index ["diary_id"], name: "index_activity_records_on_diary_id"
    t.index ["item_type", "item_id"], name: "index_activity_records_on_item_type_and_item_id"
  end

  create_table "activity_summaries", id: :serial, force: :cascade do |t|
    t.integer "owner_id"
    t.string "owner_type"
    t.integer "sum", default: 0
    t.integer "avg", default: 0
    t.integer "count", default: 1
    t.string "attr", default: "calorie"
    t.string "dur", default: "day"
    t.date "start_date"
    t.date "end_date"
    t.datetime "last_updated_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["owner_type", "owner_id"], name: "index_activity_summaries_on_owner_type_and_owner_id"
  end

  create_table "admin_users", id: :serial, force: :cascade do |t|
    t.string "email", default: "", null: false
    t.string "encrypted_password", default: "", null: false
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at"
    t.datetime "remember_created_at"
    t.integer "sign_in_count", default: 0, null: false
    t.datetime "current_sign_in_at"
    t.datetime "last_sign_in_at"
    t.inet "current_sign_in_ip"
    t.inet "last_sign_in_ip"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["email"], name: "index_admin_users_on_email", unique: true
    t.index ["reset_password_token"], name: "index_admin_users_on_reset_password_token", unique: true
  end

  create_table "announcements", id: :serial, force: :cascade do |t|
    t.string "title"
    t.string "contentable_type"
    t.integer "contentable_id"
    t.string "target_type"
    t.integer "target_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "aasm_state"
    t.integer "priority"
  end

  create_table "apple_public_keys", id: :serial, force: :cascade do |t|
    t.string "kid"
    t.string "alg"
    t.json "key"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "apps", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "display_name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "realm_id"
    t.jsonb "data"
    t.string "wechat_app_id"
    t.string "wechat_app_secret"
    t.string "wechat_app_type"
    t.string "package_name"
    t.integer "org_id"
    t.index ["name"], name: "index_apps_on_name"
  end

  create_table "articles", id: :serial, force: :cascade do |t|
    t.string "cover_photo"
    t.string "title"
    t.text "content"
    t.integer "author_id"
    t.string "backup_type"
    t.jsonb "data"
    t.datetime "display_at"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string "author_type"
    t.integer "comments_count", default: 0
    t.integer "category_id"
    t.string "aasm_state"
    t.integer "show_in_homepage", default: 0, null: false
    t.string "cover_file_name"
    t.string "cover_content_type"
    t.integer "cover_file_size"
    t.datetime "cover_updated_at"
    t.integer "org_id"
    t.integer "topic_id"
    t.integer "channel_id"
    t.string "dropbox_paper_document_url"
    t.integer "realm_id"
    t.string "subtitle"
    t.bigint "like_score", default: 0
    t.integer "likes_count", default: 0
    t.integer "shares_count", default: 0
    t.integer "sticky", default: 0
    t.bigint "hot_score", default: 0
    t.bigint "comment_score", default: 0
    t.string "pin_name"
    t.datetime "publish_datetime"
    t.datetime "published_at"
    t.integer "shows_count", default: 0
    t.string "article_type", default: "article"
    t.string "video_url"
    t.integer "shop_product_id"
    t.index ["pin_name"], name: "index_articles_on_pin_name"
  end

  create_table "batch_action_histories", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.datetime "executed_at"
    t.string "function_name"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "org_id"
    t.index ["user_id"], name: "index_batch_action_histories_on_user_id"
  end

  create_table "bloods", id: :serial, force: :cascade do |t|
    t.string "category", null: false
    t.string "name", null: false
    t.string "reference"
    t.text "tips"
    t.string "english_name"
    t.string "unit"
    t.string "source"
    t.string "remark"
  end

  create_table "body_attribute_mappings", id: :serial, force: :cascade do |t|
    t.string "key"
    t.string "value"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "provider"
    t.jsonb "data"
  end

  create_table "bot_requests", id: :serial, force: :cascade do |t|
    t.string "source"
    t.string "action"
    t.jsonb "data"
    t.boolean "finished", default: false
    t.boolean "boolean", default: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "uid"
    t.boolean "expired", default: false
    t.string "target_type"
    t.integer "target_id"
    t.index ["uid"], name: "index_bot_requests_on_uid"
  end

  create_table "branch_memberships", id: :serial, force: :cascade do |t|
    t.integer "branch_id"
    t.integer "member_id"
    t.string "member_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["branch_id"], name: "index_branch_memberships_on_branch_id"
  end

  create_table "branch_treatment_visit_relations", force: :cascade do |t|
    t.integer "treatment_id"
    t.integer "visit_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "branch_treatments", force: :cascade do |t|
    t.string "name"
    t.bigint "branch_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "client_id"
    t.date "start_date"
    t.date "end_date"
    t.index ["branch_id"], name: "index_branch_treatments_on_branch_id"
  end

  create_table "branch_visit_tasks", force: :cascade do |t|
    t.bigint "branch_id"
    t.bigint "visit_id"
    t.bigint "creater_id"
    t.bigint "assignee_id"
    t.bigint "client_id"
    t.integer "state"
    t.string "kind"
    t.string "title"
    t.date "date"
    t.datetime "start_time"
    t.datetime "finish_time"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["assignee_id"], name: "index_branch_visit_tasks_on_assignee_id"
    t.index ["branch_id"], name: "index_branch_visit_tasks_on_branch_id"
    t.index ["client_id"], name: "index_branch_visit_tasks_on_client_id"
    t.index ["creater_id"], name: "index_branch_visit_tasks_on_creater_id"
    t.index ["visit_id"], name: "index_branch_visit_tasks_on_visit_id"
  end

  create_table "branch_visits", force: :cascade do |t|
    t.bigint "client_id"
    t.bigint "branch_id"
    t.bigint "creater_id"
    t.integer "state"
    t.boolean "first_time"
    t.string "booking_source_name"
    t.string "booking_source_uid"
    t.string "kind"
    t.string "title"
    t.string "provider_name"
    t.string "service_name"
    t.date "date"
    t.datetime "time"
    t.datetime "check_in_time"
    t.datetime "finish_time"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["booking_source_name", "booking_source_uid"], name: "index_branch_visits_on_booking_source"
    t.index ["branch_id"], name: "index_branch_visits_on_branch_id"
    t.index ["client_id"], name: "index_branch_visits_on_client_id"
    t.index ["creater_id"], name: "index_branch_visits_on_creater_id"
  end

  create_table "branches", id: :serial, force: :cascade do |t|
    t.string "branch_code"
    t.string "uri"
    t.string "branch_short_name"
    t.string "address"
    t.string "phone"
    t.string "branch_time"
    t.integer "org_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.string "name"
    t.index ["org_id"], name: "index_branches_on_org_id"
  end

  create_table "campaign_actions", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.integer "org_id"
    t.integer "campaign_id"
    t.integer "campaign_stage_id"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "finished", default: false
    t.datetime "finished_at"
    t.index ["campaign_id"], name: "index_campaign_actions_on_campaign_id"
    t.index ["campaign_stage_id"], name: "index_campaign_actions_on_campaign_stage_id"
    t.index ["client_id"], name: "index_campaign_actions_on_client_id"
    t.index ["org_id"], name: "index_campaign_actions_on_org_id"
  end

  create_table "campaign_stage_settings", id: :serial, force: :cascade do |t|
    t.integer "campaign_stage_id"
    t.integer "campaign_id"
    t.integer "order"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["campaign_id"], name: "index_campaign_stage_settings_on_campaign_id"
    t.index ["campaign_stage_id"], name: "index_campaign_stage_settings_on_campaign_stage_id"
  end

  create_table "campaign_stages", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "display_name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "campaigns", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.string "name"
    t.string "display_name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "url_path"
    t.index ["org_id"], name: "index_campaigns_on_org_id"
  end

  create_table "care_relations", id: :serial, force: :cascade do |t|
    t.integer "giver_id"
    t.integer "receiver_id"
    t.string "receiver_role"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["giver_id", "receiver_id"], name: "index_care_relations_on_giver_id_and_receiver_id", unique: true
    t.index ["giver_id"], name: "index_care_relations_on_giver_id"
    t.index ["receiver_id"], name: "index_care_relations_on_receiver_id"
  end

  create_table "categories", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "org_id"
    t.integer "channel_id"
    t.jsonb "data"
  end

  create_table "channel_memberships", id: :serial, force: :cascade do |t|
    t.integer "channel_id"
    t.integer "user_id"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["channel_id"], name: "index_channel_memberships_on_channel_id"
    t.index ["user_id"], name: "index_channel_memberships_on_user_id"
  end

  create_table "channels", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.text "description"
    t.string "avatar_url"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "followers_count", default: 0, null: false
    t.integer "org_id"
    t.string "display_name"
    t.jsonb "data"
  end

  create_table "chat_announcements", id: :serial, force: :cascade do |t|
    t.jsonb "data"
    t.integer "chat_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "member_type"
    t.integer "member_id"
    t.string "aasm_state"
    t.datetime "started_at"
    t.datetime "start_date_time"
    t.index ["chat_id"], name: "index_chat_announcements_on_chat_id"
  end

  create_table "chat_attachments", id: :serial, force: :cascade do |t|
    t.integer "chat_id"
    t.integer "sender_id"
    t.string "sender_type"
    t.string "bucket"
    t.string "key"
    t.string "url"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.string "uid"
    t.index ["chat_id"], name: "index_chat_attachments_on_chat_id"
    t.index ["uid"], name: "index_chat_attachments_on_uid"
  end

  create_table "chat_members", id: :serial, force: :cascade do |t|
    t.integer "chat_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "member_type"
    t.integer "member_id"
  end

  create_table "chat_memberships", id: :serial, force: :cascade do |t|
    t.integer "chat_id"
    t.integer "member_id"
    t.string "member_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "nick_name"
    t.boolean "sync_nick_name", default: true
    t.integer "role_id"
    t.boolean "mute", default: false
    t.boolean "pinned"
    t.index ["chat_id"], name: "index_chat_memberships_on_chat_id"
    t.index ["member_id", "member_type"], name: "index_chat_memberships_on_member_id_and_member_type"
    t.index ["pinned"], name: "index_chat_memberships_on_pinned"
  end

  create_table "chats", id: :serial, force: :cascade do |t|
    t.string "status"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "chat_room_id"
    t.string "name"
    t.string "avatar_url"
    t.integer "group_class_id"
    t.string "code_name"
    t.string "created_by"
    t.integer "members_count"
    t.boolean "exclusive"
    t.datetime "latest_message_at"
    t.string "kind"
    t.string "target_type"
    t.integer "target_id"
    t.integer "org_id"
    t.string "display_name"
  end

  create_table "choices", id: :serial, force: :cascade do |t|
    t.integer "order"
    t.string "choice_content"
    t.boolean "is_free_answer"
    t.integer "question_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "sub_choice_id"
    t.string "sub_choice_content"
    t.text "score"
    t.index ["question_id"], name: "index_choices_on_question_id"
  end

  create_table "ckeditor_assets", id: :serial, force: :cascade do |t|
    t.string "data_file_name", null: false
    t.string "data_content_type"
    t.integer "data_file_size"
    t.string "data_fingerprint"
    t.string "type", limit: 30
    t.integer "width"
    t.integer "height"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["type"], name: "index_ckeditor_assets_on_type"
  end

  create_table "client_access_tokens", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.string "token_string"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "client_entries", force: :cascade do |t|
    t.bigint "client_id"
    t.bigint "measurement_id"
    t.decimal "number_value", precision: 10, scale: 4
    t.string "string_value"
    t.string "key"
    t.datetime "datetime"
    t.date "date"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id", "date"], name: "index_client_entries_on_client_id_and_date"
    t.index ["client_id", "key", "date"], name: "index_client_entries_on_client_id_and_key_and_date"
    t.index ["client_id", "key"], name: "index_client_entries_on_client_id_and_key"
    t.index ["client_id"], name: "index_client_entries_on_client_id"
    t.index ["measurement_id"], name: "index_client_entries_on_measurement_id"
  end

  create_table "client_identities", id: :serial, force: :cascade do |t|
    t.integer "app_id"
    t.integer "client_id"
    t.string "provider"
    t.string "uid"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "updatable", default: true
    t.string "password_hash"
    t.index ["app_id"], name: "index_client_identities_on_app_id"
    t.index ["client_id"], name: "index_client_identities_on_client_id"
  end

  create_table "client_memos", force: :cascade do |t|
    t.bigint "client_id"
    t.bigint "user_id"
    t.string "record_type"
    t.bigint "record_id"
    t.text "text"
    t.string "title"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "comments_count", default: 0
    t.jsonb "data"
    t.index ["client_id"], name: "index_client_memos_on_client_id"
    t.index ["record_type", "record_id"], name: "index_client_memos_on_record_type_and_record_id"
    t.index ["user_id"], name: "index_client_memos_on_user_id"
  end

  create_table "clients", id: :serial, force: :cascade do |t|
    t.string "name", limit: 255, null: false
    t.integer "user_id"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.date "birthday", null: false
    t.integer "consulting_group_id", default: 1, null: false
    t.string "gender", default: "female", null: false
    t.string "education"
    t.string "occupations"
    t.string "tel"
    t.string "mobile"
    t.string "email"
    t.string "address"
    t.string "insurance"
    t.text "allergen_intolerance"
    t.boolean "drinking"
    t.string "drinking_content"
    t.boolean "smoking"
    t.string "smoking_content"
    t.boolean "areaca"
    t.string "areaca_content"
    t.text "dietary_pattern"
    t.string "nutition_supplements"
    t.string "not_like_food"
    t.string "like_food"
    t.string "exercise_habits"
    t.string "would_exercise_habits"
    t.text "disease"
    t.text "disease_content"
    t.text "drug_case"
    t.text "customer_records"
    t.text "functional"
    t.json "family_history"
    t.text "note"
    t.boolean "star", default: false
    t.string "metric", default: "metric", null: false
    t.string "unit", default: "kcal", null: false
    t.text "reports_reports"
    t.text "customer_records_reports"
    t.json "pregnant"
    t.string "first_name"
    t.string "last_name"
    t.string "encrypted_password"
    t.integer "sign_in_count", default: 0
    t.datetime "current_sign_in_at"
    t.string "current_sign_in_ip"
    t.string "last_sign_in_at"
    t.string "last_sign_in_ip"
    t.string "confirmation_token"
    t.datetime "confirmed_at"
    t.datetime "confirmation_sent_at"
    t.string "avatar_file_name"
    t.string "avatar_content_type"
    t.integer "avatar_file_size"
    t.string "reserve_email"
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at"
    t.datetime "remember_created_at"
    t.string "unconfirmed_email"
    t.string "provider"
    t.string "uid", default: "", null: false
    t.text "tokens"
    t.string "image"
    t.jsonb "extra"
    t.jsonb "current"
    t.string "source"
    t.boolean "offline", default: false
    t.integer "followings_count", default: 0, null: false
    t.integer "followers_count", default: 0, null: false
    t.string "uuid"
    t.string "avatar_url"
    t.string "locale"
    t.string "identity_number"
    t.integer "posts_count", default: 0
    t.string "nick_name"
    t.boolean "allow_password_change", default: false
    t.string "real_name"
    t.index ["consulting_group_id"], name: "index_clients_on_consulting_group_id"
    t.index ["email"], name: "index_clients_on_email"
    t.index ["first_name"], name: "index_clients_on_first_name"
    t.index ["identity_number"], name: "index_clients_on_identity_number"
    t.index ["last_name"], name: "index_clients_on_last_name"
  end

  create_table "clinic_prescription_medication_orders", force: :cascade do |t|
    t.bigint "prescription_id"
    t.bigint "medicine_id"
    t.decimal "single_amount", precision: 6, scale: 2
    t.string "frequency"
    t.decimal "frequency_ratio", precision: 6, scale: 4
    t.integer "days"
    t.decimal "total_amount", precision: 6, scale: 2
    t.decimal "medicine_amount", precision: 6, scale: 2
    t.text "compliance"
    t.boolean "archived", default: false
    t.boolean "owed", default: false
    t.boolean "returned", default: false
    t.string "return_reason"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "output_required", default: true
    t.string "output_status"
    t.boolean "archivable", default: true
    t.decimal "output_amount", precision: 6, scale: 2, default: "0.0"
    t.decimal "outputable_amount", precision: 6, scale: 2, default: "0.0"
    t.index ["archived"], name: "index_clinic_prescription_medication_orders_on_archived"
    t.index ["medicine_id"], name: "index_clinic_prescription_medication_orders_on_medicine_id"
    t.index ["owed"], name: "index_clinic_prescription_medication_orders_on_owed"
    t.index ["prescription_id"], name: "index_clinic_prescription_medication_orders_on_prescription_id"
    t.index ["returned"], name: "index_clinic_prescription_medication_orders_on_returned"
  end

  create_table "clinic_prescriptions", force: :cascade do |t|
    t.date "date"
    t.bigint "user_id"
    t.bigint "soap_id"
    t.bigint "client_id"
    t.boolean "archived", default: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "output_status"
    t.boolean "owed", default: false
    t.boolean "returned", default: false
    t.boolean "output_required", default: true
    t.boolean "archivable", default: true
    t.string "serial_number"
    t.integer "branch_id"
    t.boolean "mo_archived", default: false
    t.integer "last_update_user_id"
    t.datetime "last_update_datetime"
    t.index ["archived"], name: "index_clinic_prescriptions_on_archived"
    t.index ["branch_id", "date"], name: "index_clinic_prescriptions_on_branch_id_and_date"
    t.index ["branch_id", "serial_number"], name: "index_clinic_prescriptions_on_branch_id_and_serial_number"
    t.index ["client_id"], name: "index_clinic_prescriptions_on_client_id"
    t.index ["soap_id"], name: "index_clinic_prescriptions_on_soap_id"
    t.index ["user_id"], name: "index_clinic_prescriptions_on_user_id"
  end

  create_table "clinic_soaps", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.jsonb "subjective"
    t.jsonb "objective"
    t.jsonb "assessment"
    t.jsonb "plan"
    t.text "dr_comment"
    t.text "pre_rd_comment"
    t.text "post_rd_comment"
    t.text "pharmacist_comment"
    t.text "nurse_comment"
    t.integer "creater_id"
    t.string "creater_type"
    t.integer "last_updater_id"
    t.string "last_updater_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.date "date"
    t.string "external_url"
    t.string "external_id"
    t.integer "branch_id"
    t.index ["client_id"], name: "index_clinic_soaps_on_client_id"
  end

  create_table "coin_accounts", id: :serial, force: :cascade do |t|
    t.integer "balance", default: 0, null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "owner_id"
    t.string "owner_type"
    t.index ["owner_type", "owner_id"], name: "index_coin_accounts_on_owner_type_and_owner_id"
  end

  create_table "coin_deposits", id: :serial, force: :cascade do |t|
    t.integer "account_id", null: false
    t.integer "depositable_id"
    t.string "depositable_type"
    t.integer "balance", null: false
    t.integer "amount", null: false
    t.string "status"
    t.datetime "expires_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "owner_id"
    t.string "owner_type"
    t.boolean "creditable", default: false
    t.index ["account_id"], name: "index_coin_deposits_on_account_id"
    t.index ["depositable_type", "depositable_id"], name: "index_coin_deposits_on_depositable_type_and_depositable_id"
    t.index ["owner_type", "owner_id"], name: "index_coin_deposits_on_owner_type_and_owner_id"
  end

  create_table "coin_events", id: :serial, force: :cascade do |t|
    t.integer "deposit_id", null: false
    t.integer "eventable_id"
    t.string "eventable_type"
    t.integer "amount", null: false
    t.string "kind"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["deposit_id"], name: "index_coin_events_on_deposit_id"
    t.index ["eventable_type", "eventable_id"], name: "index_coin_events_on_eventable_type_and_eventable_id"
  end

  create_table "coin_transactions", id: :serial, force: :cascade do |t|
    t.integer "deposit_id", null: false
    t.integer "event_id", null: false
    t.integer "amount", default: 0, null: false
    t.string "type", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["deposit_id"], name: "index_coin_transactions_on_deposit_id"
    t.index ["event_id"], name: "index_coin_transactions_on_event_id"
  end

  create_table "comments", id: :serial, force: :cascade do |t|
    t.integer "sender_id"
    t.string "sender_type"
    t.integer "record_id"
    t.string "record_type"
    t.string "content"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.index ["record_id", "record_type"], name: "index_comments_on_record_id_and_record_type"
    t.index ["sender_id", "sender_type"], name: "index_comments_on_sender_id_and_sender_type"
  end

  create_table "completes", id: :serial, force: :cascade do |t|
    t.string "completeable_type"
    t.integer "completeable_id"
    t.integer "client_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id", "completeable_type", "completeable_id"], name: "completed"
  end

  create_table "connected_devices", id: :serial, force: :cascade do |t|
    t.integer "member_id"
    t.string "member_type"
    t.string "uid"
    t.string "name"
    t.string "vendor"
    t.string "display_name"
    t.string "connection_type"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "connection_maps", id: :serial, force: :cascade do |t|
    t.string "provider"
    t.string "branch_name"
    t.string "branch_code"
    t.integer "dietician_id"
    t.integer "physical_fitness_instructor_id"
    t.integer "branch_manager_id"
    t.integer "nurse_id"
    t.integer "physiotherapist_id"
    t.integer "doctor_id"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "consulting_client_summaries", id: :serial, force: :cascade do |t|
    t.date "date", null: false
    t.decimal "height", precision: 4, scale: 1
    t.decimal "weight", precision: 4, scale: 1
    t.decimal "bmi"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "client_id"
    t.decimal "waist_circumference"
    t.decimal "hip_circumference"
    t.decimal "waist_hip_ratio"
    t.decimal "body_fat_mass"
    t.decimal "body_fat_mass_percentage"
    t.decimal "lean_body_mass"
    t.decimal "lean_body_mass_percentage"
    t.decimal "total_body_water"
    t.decimal "bmr"
    t.decimal "bee"
    t.decimal "af"
    t.decimal "systolic_blood_pressure"
    t.decimal "diastolic_blood_pressure"
    t.decimal "pulse"
    t.decimal "blood_sugar"
    t.decimal "total_cholesterol"
    t.decimal "triglyceride"
    t.decimal "low_density_lipoprotein_cholesterol"
    t.decimal "high_density_lipoprotein"
    t.decimal "cholesterol"
    t.decimal "uric_acid"
    t.string "edema", default: ""
    t.decimal "ee"
    t.text "blood"
    t.integer "sf"
    t.string "af_bk"
    t.jsonb "data"
    t.integer "source_type", default: 0, null: false
    t.decimal "muscle_mass_percentage"
    t.integer "user_id"
    t.index ["client_id"], name: "index_consulting_client_summaries_on_client_id"
  end

  create_table "consulting_client_targets", id: :serial, force: :cascade do |t|
    t.integer "client_id", null: false
    t.date "begin_date", null: false
    t.date "end_date", null: false
    t.integer "water"
    t.integer "kg"
    t.integer "protein"
    t.integer "fat"
    t.integer "carbohydrate"
    t.decimal "calorie", precision: 11, scale: 5
    t.decimal "vita_iu", precision: 11, scale: 5
    t.decimal "b1", precision: 11, scale: 5
    t.decimal "b2", precision: 11, scale: 5
    t.decimal "b6", precision: 11, scale: 5
    t.decimal "b12", precision: 11, scale: 5
    t.integer "niacin"
    t.integer "folicacid"
    t.decimal "vitC", precision: 11, scale: 5
    t.integer "dietary_fiber"
    t.integer "na"
    t.integer "ca"
    t.integer "fe"
    t.integer "mg"
    t.integer "k"
    t.integer "zn"
    t.integer "p"
    t.integer "i"
    t.decimal "pantothenic_acid", precision: 11, scale: 5
    t.decimal "biotin", precision: 11, scale: 5
    t.integer "choline"
    t.decimal "f", precision: 11, scale: 5
    t.decimal "se", precision: 11, scale: 5
    t.decimal "vitD", precision: 11, scale: 5
    t.decimal "vitE", precision: 11, scale: 5
    t.decimal "vitK", precision: 11, scale: 5
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "weekly"
    t.string "target_weight"
    t.decimal "fruits", precision: 4, scale: 1, default: "0.0"
    t.decimal "vegetables", precision: 4, scale: 1, default: "0.0"
    t.decimal "grains", precision: 4, scale: 1, default: "0.0"
    t.decimal "meat_beans_low_fat", precision: 4, scale: 1, default: "0.0"
    t.decimal "meat_beans_medium_fat", precision: 4, scale: 1, default: "0.0"
    t.decimal "meat_beans_high_fat", precision: 4, scale: 1, default: "0.0"
    t.decimal "milk_whole_fat", precision: 4, scale: 1, default: "0.0"
    t.decimal "milk_low_fat", precision: 4, scale: 1, default: "0.0"
    t.decimal "milk_skim", precision: 4, scale: 1, default: "0.0"
    t.decimal "oil", precision: 4, scale: 1, default: "0.0"
    t.decimal "fiber", precision: 4, scale: 1
    t.decimal "total_sugars", precision: 4, scale: 1
    t.decimal "cholesterol", precision: 4, scale: 1
    t.string "kind"
    t.decimal "start_weight", precision: 4, scale: 1
    t.text "memo"
    t.index ["client_id"], name: "index_consulting_client_targets_on_client_id"
  end

  create_table "consulting_food_record_items", id: :serial, force: :cascade do |t|
    t.integer "consulting_food_record_id", null: false
    t.integer "item_id"
    t.decimal "amount", default: "1.0", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "meal", limit: 2, default: 1, null: false
    t.string "item_type", limit: 255, null: false
    t.datetime "added_at"
    t.integer "photo_id"
    t.jsonb "nutrient"
    t.integer "note_id"
    t.index ["consulting_food_record_id"], name: "index_consulting_food_record_items_on_consulting_food_record_id"
    t.index ["item_id", "item_type"], name: "index_consulting_food_record_items_on_item_id_and_item_type"
  end

  create_table "consulting_food_record_meals", id: :serial, force: :cascade do |t|
    t.integer "consulting_food_record_id"
    t.integer "meal", limit: 2
    t.string "photo"
    t.text "description"
    t.string "store"
    t.string "location"
    t.integer "like"
    t.string "tag"
    t.decimal "calorie", precision: 11, scale: 5
    t.integer "protein"
    t.integer "fat"
    t.integer "carbohydrate"
    t.decimal "fruits", precision: 4, scale: 1
    t.decimal "vegetables", precision: 4, scale: 1
    t.decimal "grains", precision: 4, scale: 1
    t.decimal "meat_beans_low_fat", precision: 4, scale: 1
    t.decimal "meat_beans_medium_fat", precision: 4, scale: 1
    t.decimal "meat_beans_high_fat", precision: 4, scale: 1
    t.decimal "meat_beans_super_high_fat", precision: 4, scale: 1
    t.decimal "milk_whole_fat", precision: 4, scale: 1
    t.decimal "milk_low_fat", precision: 4, scale: 1
    t.decimal "milk_skim", precision: 4, scale: 1
    t.decimal "oil", precision: 4, scale: 1
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["consulting_food_record_id"], name: "index_consulting_food_record_meals_on_consulting_food_record_id"
  end

  create_table "consulting_food_record_notes", id: :serial, force: :cascade do |t|
    t.string "description"
    t.string "store"
    t.string "location"
    t.integer "like"
    t.string "light"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "nutrient_summary"
    t.integer "meal", limit: 2, default: 1
    t.datetime "taken_at"
    t.integer "comments_count", default: 0
    t.integer "client_id"
    t.integer "food_record_id"
    t.integer "items_count", default: 0
    t.index ["client_id"], name: "index_consulting_food_record_notes_on_client_id"
    t.index ["food_record_id"], name: "index_consulting_food_record_notes_on_food_record_id"
  end

  create_table "consulting_food_record_photos", id: :serial, force: :cascade do |t|
    t.string "url"
    t.string "bucket"
    t.string "name"
    t.string "description"
    t.integer "size"
    t.integer "client_id"
    t.integer "meal_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "store"
    t.string "location"
    t.integer "like"
    t.string "light"
    t.jsonb "nutrient_summary"
    t.integer "consulting_food_record_id"
    t.integer "comments_count", default: 0
    t.integer "meal", limit: 2, default: 1
    t.datetime "taken_at"
    t.integer "items_count", default: 0
    t.index ["client_id"], name: "index_consulting_food_record_photos_on_client_id"
    t.index ["consulting_food_record_id"], name: "index_consulting_food_record_photos_on_record_id"
    t.index ["meal_id"], name: "index_consulting_food_record_photos_on_meal_id"
  end

  create_table "consulting_food_records", id: :serial, force: :cascade do |t|
    t.string "name", limit: 255, null: false
    t.string "record_type", limit: 20, default: "dieting_record", null: false
    t.integer "client_id", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "water_intake"
    t.string "mood_diary"
    t.text "daily_note"
    t.string "dietitain_note"
    t.text "tips"
    t.date "date", null: false
    t.text "exercise_type"
    t.decimal "calorie"
    t.decimal "fat"
    t.decimal "protein"
    t.decimal "carbohydrate"
    t.json "food_item_data"
    t.jsonb "custom_items"
    t.integer "items_count", default: 0
    t.integer "notes_count"
    t.integer "photos_count"
    t.index ["client_id"], name: "index_consulting_food_records_on_client_id"
    t.index ["date", "client_id"], name: "index_consulting_food_records_on_date_and_client_id"
    t.index ["date"], name: "index_consulting_food_records_on_date"
  end

  create_table "consulting_groups", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.integer "user_id", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["user_id"], name: "index_consulting_groups_on_user_id"
  end

  create_table "consulting_plans", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.string "code", null: false
    t.decimal "amount", precision: 10, scale: 2, null: false
    t.string "currency", default: "NTD", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "interval"
    t.integer "interval_count"
    t.integer "client_limit"
    t.integer "connected_client_limit"
    t.integer "archived_client_limit"
  end

  create_table "consulting_subscription_billings", id: :serial, force: :cascade do |t|
    t.string "number", null: false
    t.string "status", null: false
    t.string "handler_number"
    t.string "handler"
    t.string "payment_type", null: false
    t.decimal "amount", precision: 10, scale: 2, null: false
    t.decimal "external_fee", precision: 10, scale: 2, default: "0.0"
    t.string "currency", default: "NTD", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.datetime "traded_at"
    t.string "handler_message"
    t.integer "subscription_id"
    t.date "period_start_date"
    t.index ["subscription_id"], name: "index_consulting_subscription_billings_on_subscription_id"
  end

  create_table "consulting_subscriptions", id: :serial, force: :cascade do |t|
    t.string "number", null: false
    t.string "status", null: false
    t.string "handler"
    t.string "interval"
    t.integer "interval_count"
    t.integer "credit_card_exec_times"
    t.decimal "amount", precision: 10, scale: 2, null: false
    t.string "currency", default: "NTD", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "user_id"
    t.string "plan_name"
    t.integer "plan_id"
    t.string "payment_type"
    t.date "started_on"
    t.date "ended_on"
    t.date "current_period_start"
    t.date "current_period_end"
    t.string "verify_number"
    t.string "tax_id"
    t.integer "invoice_type"
    t.string "invoice_title"
    t.index ["plan_id"], name: "index_consulting_subscriptions_on_plan_id"
    t.index ["user_id"], name: "index_consulting_subscriptions_on_user_id"
  end

  create_table "course_set_course_relationships", id: :serial, force: :cascade do |t|
    t.integer "course_set_id"
    t.integer "course_id"
    t.integer "week"
    t.integer "day"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["course_id"], name: "index_course_set_course_relationships_on_course_id"
    t.index ["course_set_id"], name: "index_course_set_course_relationships_on_course_set_id"
  end

  create_table "course_set_template_course_relationships", id: :serial, force: :cascade do |t|
    t.integer "course_set_template_id"
    t.integer "course_id"
    t.integer "week"
    t.integer "day"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "course_set_templates", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "course_sets", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "group_class_id"
  end

  create_table "courses", id: :serial, force: :cascade do |t|
    t.string "title"
    t.text "content"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "user_id"
    t.string "cover_file_name"
    t.string "cover_content_type"
    t.integer "cover_file_size"
    t.datetime "cover_updated_at"
    t.string "aasm_state"
    t.integer "comments_count", default: 0, null: false
    t.integer "likes_count", default: 0, null: false
    t.integer "shows_count", default: 0, null: false
    t.string "dropbox_paper_document_url"
    t.jsonb "data"
    t.string "subtitle"
    t.integer "completes_count"
    t.integer "questions_count", default: 0
    t.string "cover_photo"
    t.string "provider"
    t.integer "org_id"
  end

  create_table "diseases", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.text "blood"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "ecpay_orders", id: :serial, force: :cascade do |t|
    t.string "merchant_trade_no"
    t.integer "total_amount"
    t.string "trade_desc"
    t.string "item_name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "aasm_state"
    t.jsonb "data"
  end

  create_table "event_essays_to_whytewoolves", id: :serial, force: :cascade do |t|
  end

  create_table "feedbacks", id: :serial, force: :cascade do |t|
    t.integer "member_id"
    t.string "member_type"
    t.integer "app_id"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "follows", id: :serial, force: :cascade do |t|
    t.integer "follower_id"
    t.string "follower_type"
    t.integer "followed_id"
    t.string "followed_type"
    t.datetime "created_at", null: false
    t.index ["followed_type", "followed_id", "follower_type", "follower_id"], name: "unique_follow", unique: true
    t.index ["followed_type", "followed_id"], name: "index_follows_on_followed_type_and_followed_id"
    t.index ["follower_type", "follower_id"], name: "index_follows_on_follower_type_and_follower_id"
  end

  create_table "food_calculator_product_ingredients", id: :serial, force: :cascade do |t|
    t.integer "food_calculator_product_id", null: false
    t.integer "ingredient_id", null: false
    t.decimal "amount", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "food_calculator_products", id: :serial, force: :cascade do |t|
    t.integer "food_calculator_project_id", null: false
    t.string "name", limit: 255, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "position", null: false
  end

  create_table "food_calculator_project_notes", id: :serial, force: :cascade do |t|
    t.integer "food_calculator_project_id", null: false
    t.string "content", limit: 800, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "food_calculator_projects", id: :serial, force: :cascade do |t|
    t.string "name", limit: 255, null: false
    t.integer "user_id", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "position", null: false
  end

  create_table "food_ingredients", id: :serial, force: :cascade do |t|
    t.integer "ingredient_id", null: false
    t.decimal "amount", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "food_stuff_backups", force: :cascade do |t|
    t.string "catalogue"
    t.string "name", null: false
    t.string "english_name"
    t.string "description"
    t.decimal "wastage_rate", precision: 11, scale: 5, default: "-1.0"
    t.decimal "calorie", precision: 11, scale: 5, default: "-1.0"
    t.decimal "water", precision: 11, scale: 5, default: "-1.0"
    t.decimal "protein", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ash", precision: 11, scale: 5, default: "-1.0"
    t.decimal "carbohydrate", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fiber", precision: 11, scale: 5, default: "-1.0"
    t.decimal "dietary_fiber", precision: 11, scale: 5, default: "-1.0"
    t.decimal "total_sugars", precision: 11, scale: 5, default: "-1.0"
    t.decimal "glucose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fructose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "maltose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "sucrose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lactose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "na", precision: 11, scale: 5, default: "-1.0"
    t.decimal "k", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ca", precision: 11, scale: 5, default: "-1.0"
    t.decimal "mg", precision: 11, scale: 5, default: "-1.0"
    t.decimal "p", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fe", precision: 11, scale: 5, default: "-1.0"
    t.decimal "zn", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b1", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b2", precision: 11, scale: 5, default: "-1.0"
    t.decimal "niacin", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b6", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b12", precision: 11, scale: 5, default: "-1.0"
    t.decimal "folicacid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitc", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_te", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vite", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "y_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "g_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_saturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "butyric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "caprylic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "capric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lauric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "tridecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "myristic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pentadecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "palmitic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "margaric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "stearic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "nonadecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arachidic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "behenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lignoceric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_monounsaturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "myristoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "palmitoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "oleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "gadoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "erucic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "linoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_polyunsaturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "linolenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "octadecatetraenoicacid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arachidonic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "eicosapentaenoic_acid_epa", precision: 11, scale: 5, default: "-1.0"
    t.decimal "docosapentaenoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "docosahexaenoic_acid_dha", precision: 11, scale: 5, default: "-1.0"
    t.string "p_m_s", default: "-1.0"
    t.decimal "cholesterol", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vita_iu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "retinol_equivalentre", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_carotene", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b_carotene", precision: 11, scale: 5, default: "-1.0"
    t.decimal "retinol", precision: 11, scale: 5, default: "-1.0"
    t.decimal "amino_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "asp", precision: 11, scale: 5, default: "-1.0"
    t.decimal "thr", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ser", precision: 11, scale: 5, default: "-1.0"
    t.decimal "glu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pro", precision: 11, scale: 5, default: "-1.0"
    t.decimal "gly", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ala", precision: 11, scale: 5, default: "-1.0"
    t.decimal "cys", precision: 11, scale: 5, default: "-1.0"
    t.decimal "val", precision: 11, scale: 5, default: "-1.0"
    t.decimal "met", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ile", precision: 11, scale: 5, default: "-1.0"
    t.decimal "leu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "tyr", precision: 11, scale: 5, default: "-1.0"
    t.decimal "phe", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lys", precision: 11, scale: 5, default: "-1.0"
    t.decimal "his", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arg", precision: 11, scale: 5, default: "-1.0"
    t.decimal "trp", precision: 11, scale: 5, default: "-1.0"
    t.decimal "other", precision: 11, scale: 5, default: "-1.0"
    t.decimal "alcohol_content", precision: 11, scale: 5, default: "-1.0"
    t.decimal "revise_calorie", precision: 11, scale: 5, default: "-1.0"
    t.decimal "serving_weight", precision: 11, scale: 5, default: "-1.0"
    t.string "serving_unit"
    t.integer "display_factor", null: false
    t.integer "six_group_factor"
    t.decimal "fruits", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vegetables", precision: 11, scale: 5, default: "-1.0"
    t.decimal "grains", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_low_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_medium_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_high_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_super_high_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_whole_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_low_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_skim", precision: 11, scale: 5, default: "-1.0"
    t.decimal "oil", precision: 11, scale: 5, default: "-1.0"
    t.string "source", null: false
    t.string "brand_group"
    t.string "brand"
    t.string "type_or_place"
    t.string "high_fe"
    t.string "low_purine"
    t.string "low_protein"
    t.string "high_mg"
    t.string "high_k"
    t.string "high_ca"
    t.string "high_fiber"
    t.string "non_toxic"
    t.string "organic"
    t.integer "count", default: 0
    t.text "remarks"
    t.string "common_name"
    t.string "low_na"
    t.decimal "ex_weight", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitD", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitK", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pantothenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "choline", precision: 11, scale: 5, default: "-1.0"
    t.decimal "biotin", precision: 11, scale: 5, default: "-1.0"
    t.decimal "i", precision: 11, scale: 5, default: "-1.0"
    t.decimal "se", precision: 11, scale: 5, default: "-1.0"
    t.decimal "f", precision: 11, scale: 5, default: "-1.0"
    t.integer "sort"
    t.integer "weight_factor", null: false
    t.decimal "multiply_ratio", precision: 4, scale: 2
    t.string "image_url"
    t.boolean "display", default: true
    t.jsonb "units"
    t.string "catalogue_zh_cn"
    t.string "name_zh_cn"
    t.string "serving_unit_zh_cn"
    t.string "brand_zh_cn"
    t.decimal "tans_fat", precision: 11, scale: 5
    t.decimal "vitD_iu", precision: 11, scale: 5
    t.decimal "vitD_ug", precision: 11, scale: 5
    t.decimal "vitD_2", precision: 11, scale: 5
    t.decimal "vitD_3", precision: 11, scale: 5
    t.decimal "vitK_1", precision: 11, scale: 5
    t.decimal "vitK_2", precision: 11, scale: 5
    t.decimal "cu", precision: 11, scale: 5
    t.decimal "mn", precision: 11, scale: 5
    t.decimal "caproic_acid", precision: 11, scale: 5
    t.decimal "fatty_acids_other", precision: 11, scale: 5
    t.decimal "galactose", precision: 11, scale: 5
    t.decimal "chen_Isfl_Val", precision: 11, scale: 5
    t.decimal "chen_isf710", precision: 11, scale: 5
    t.decimal "chen_isf711", precision: 11, scale: 5
    t.decimal "chen_isf712", precision: 11, scale: 5
    t.decimal "chen_isf713", precision: 11, scale: 5
    t.decimal "chen_isf714", precision: 11, scale: 5
    t.decimal "chen_isf715", precision: 11, scale: 5
    t.decimal "chen_isf716", precision: 11, scale: 5
    t.decimal "chen_F731", precision: 11, scale: 5
    t.decimal "chen_F741", precision: 11, scale: 5
    t.decimal "chen_F742", precision: 11, scale: 5
    t.decimal "chen_F743", precision: 11, scale: 5
    t.decimal "chen_F745", precision: 11, scale: 5
    t.decimal "chen_F746", precision: 11, scale: 5
    t.decimal "chen_F749", precision: 11, scale: 5
    t.decimal "chen_F750", precision: 11, scale: 5
    t.decimal "chen_F751", precision: 11, scale: 5
    t.decimal "chen_F752", precision: 11, scale: 5
    t.decimal "chen_F753", precision: 11, scale: 5
    t.decimal "chen_F755", precision: 11, scale: 5
    t.decimal "chen_F756", precision: 11, scale: 5
    t.decimal "chen_F758", precision: 11, scale: 5
    t.decimal "chen_F759", precision: 11, scale: 5
    t.decimal "chen_F762", precision: 11, scale: 5
    t.decimal "chen_F770", precision: 11, scale: 5
    t.decimal "chen_F773", precision: 11, scale: 5
    t.decimal "chen_F785", precision: 11, scale: 5
    t.decimal "chen_F786", precision: 11, scale: 5
    t.decimal "chen_F788", precision: 11, scale: 5
    t.decimal "chen_F789", precision: 11, scale: 5
    t.decimal "chen_F790", precision: 11, scale: 5
    t.decimal "chen_F791", precision: 11, scale: 5
    t.decimal "chen_F792", precision: 11, scale: 5
    t.decimal "chen_F793", precision: 11, scale: 5
    t.decimal "chen_F794", precision: 11, scale: 5
    t.decimal "chen_F795", precision: 11, scale: 5
    t.decimal "chen_ala", precision: 11, scale: 5
    t.decimal "chen_epa", precision: 11, scale: 5
    t.decimal "chen_dha", precision: 11, scale: 5
    t.decimal "chen_n3_total", precision: 11, scale: 5
    t.decimal "chen_n3_mean", precision: 11, scale: 5
    t.decimal "chen_VitD", precision: 11, scale: 5
    t.decimal "chen_VitD_mean", precision: 11, scale: 5
    t.decimal "vitK_2_4", precision: 11, scale: 5
    t.decimal "vitK_2_7", precision: 11, scale: 5
    t.decimal "fatty_acids_saturated", precision: 11, scale: 5
  end

  create_table "food_stuff_dry_runs", force: :cascade do |t|
    t.string "catalogue"
    t.string "name", null: false
    t.string "english_name"
    t.string "description"
    t.decimal "wastage_rate", precision: 11, scale: 5, default: "-1.0"
    t.decimal "calorie", precision: 11, scale: 5, default: "-1.0"
    t.decimal "water", precision: 11, scale: 5, default: "-1.0"
    t.decimal "protein", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ash", precision: 11, scale: 5, default: "-1.0"
    t.decimal "carbohydrate", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fiber", precision: 11, scale: 5, default: "-1.0"
    t.decimal "dietary_fiber", precision: 11, scale: 5, default: "-1.0"
    t.decimal "total_sugars", precision: 11, scale: 5, default: "-1.0"
    t.decimal "glucose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fructose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "maltose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "sucrose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lactose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "na", precision: 11, scale: 5, default: "-1.0"
    t.decimal "k", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ca", precision: 11, scale: 5, default: "-1.0"
    t.decimal "mg", precision: 11, scale: 5, default: "-1.0"
    t.decimal "p", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fe", precision: 11, scale: 5, default: "-1.0"
    t.decimal "zn", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b1", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b2", precision: 11, scale: 5, default: "-1.0"
    t.decimal "niacin", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b6", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b12", precision: 11, scale: 5, default: "-1.0"
    t.decimal "folicacid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitc", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_te", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vite", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "y_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "g_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_saturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "butyric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "caprylic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "capric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lauric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "tridecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "myristic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pentadecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "palmitic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "margaric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "stearic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "nonadecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arachidic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "behenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lignoceric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_monounsaturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "myristoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "palmitoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "oleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "gadoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "erucic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "linoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_polyunsaturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "linolenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "octadecatetraenoicacid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arachidonic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "eicosapentaenoic_acid_epa", precision: 11, scale: 5, default: "-1.0"
    t.decimal "docosapentaenoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "docosahexaenoic_acid_dha", precision: 11, scale: 5, default: "-1.0"
    t.string "p_m_s", default: "-1.0"
    t.decimal "cholesterol", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vita_iu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "retinol_equivalentre", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_carotene", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b_carotene", precision: 11, scale: 5, default: "-1.0"
    t.decimal "retinol", precision: 11, scale: 5, default: "-1.0"
    t.decimal "amino_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "asp", precision: 11, scale: 5, default: "-1.0"
    t.decimal "thr", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ser", precision: 11, scale: 5, default: "-1.0"
    t.decimal "glu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pro", precision: 11, scale: 5, default: "-1.0"
    t.decimal "gly", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ala", precision: 11, scale: 5, default: "-1.0"
    t.decimal "cys", precision: 11, scale: 5, default: "-1.0"
    t.decimal "val", precision: 11, scale: 5, default: "-1.0"
    t.decimal "met", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ile", precision: 11, scale: 5, default: "-1.0"
    t.decimal "leu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "tyr", precision: 11, scale: 5, default: "-1.0"
    t.decimal "phe", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lys", precision: 11, scale: 5, default: "-1.0"
    t.decimal "his", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arg", precision: 11, scale: 5, default: "-1.0"
    t.decimal "trp", precision: 11, scale: 5, default: "-1.0"
    t.decimal "other", precision: 11, scale: 5, default: "-1.0"
    t.decimal "alcohol_content", precision: 11, scale: 5, default: "-1.0"
    t.decimal "revise_calorie", precision: 11, scale: 5, default: "-1.0"
    t.decimal "serving_weight", precision: 11, scale: 5, default: "-1.0"
    t.string "serving_unit"
    t.integer "display_factor", null: false
    t.integer "six_group_factor"
    t.decimal "fruits", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vegetables", precision: 11, scale: 5, default: "-1.0"
    t.decimal "grains", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_low_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_medium_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_high_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_super_high_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_whole_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_low_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_skim", precision: 11, scale: 5, default: "-1.0"
    t.decimal "oil", precision: 11, scale: 5, default: "-1.0"
    t.string "source", null: false
    t.string "brand_group"
    t.string "brand"
    t.string "type_or_place"
    t.string "high_fe"
    t.string "low_purine"
    t.string "low_protein"
    t.string "high_mg"
    t.string "high_k"
    t.string "high_ca"
    t.string "high_fiber"
    t.string "non_toxic"
    t.string "organic"
    t.integer "count", default: 0
    t.text "remarks"
    t.string "common_name"
    t.string "low_na"
    t.decimal "ex_weight", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitD", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitK", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pantothenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "choline", precision: 11, scale: 5, default: "-1.0"
    t.decimal "biotin", precision: 11, scale: 5, default: "-1.0"
    t.decimal "i", precision: 11, scale: 5, default: "-1.0"
    t.decimal "se", precision: 11, scale: 5, default: "-1.0"
    t.decimal "f", precision: 11, scale: 5, default: "-1.0"
    t.integer "sort"
    t.integer "weight_factor", null: false
    t.decimal "multiply_ratio", precision: 4, scale: 2
    t.string "image_url"
    t.boolean "display", default: true
    t.jsonb "units"
    t.string "catalogue_zh_cn"
    t.string "name_zh_cn"
    t.string "serving_unit_zh_cn"
    t.string "brand_zh_cn"
    t.decimal "tans_fat", precision: 11, scale: 5
    t.decimal "vitD_iu", precision: 11, scale: 5
    t.decimal "vitD_ug", precision: 11, scale: 5
    t.decimal "vitD_2", precision: 11, scale: 5
    t.decimal "vitD_3", precision: 11, scale: 5
    t.decimal "vitK_1", precision: 11, scale: 5
    t.decimal "vitK_2", precision: 11, scale: 5
    t.decimal "cu", precision: 11, scale: 5
    t.decimal "mn", precision: 11, scale: 5
    t.decimal "caproic_acid", precision: 11, scale: 5
    t.decimal "fatty_acids_other", precision: 11, scale: 5
    t.decimal "galactose", precision: 11, scale: 5
    t.decimal "chen_Isfl_Val", precision: 11, scale: 5
    t.decimal "chen_isf710", precision: 11, scale: 5
    t.decimal "chen_isf711", precision: 11, scale: 5
    t.decimal "chen_isf712", precision: 11, scale: 5
    t.decimal "chen_isf713", precision: 11, scale: 5
    t.decimal "chen_isf714", precision: 11, scale: 5
    t.decimal "chen_isf715", precision: 11, scale: 5
    t.decimal "chen_isf716", precision: 11, scale: 5
    t.decimal "chen_F731", precision: 11, scale: 5
    t.decimal "chen_F741", precision: 11, scale: 5
    t.decimal "chen_F742", precision: 11, scale: 5
    t.decimal "chen_F743", precision: 11, scale: 5
    t.decimal "chen_F745", precision: 11, scale: 5
    t.decimal "chen_F746", precision: 11, scale: 5
    t.decimal "chen_F749", precision: 11, scale: 5
    t.decimal "chen_F750", precision: 11, scale: 5
    t.decimal "chen_F751", precision: 11, scale: 5
    t.decimal "chen_F752", precision: 11, scale: 5
    t.decimal "chen_F753", precision: 11, scale: 5
    t.decimal "chen_F755", precision: 11, scale: 5
    t.decimal "chen_F756", precision: 11, scale: 5
    t.decimal "chen_F758", precision: 11, scale: 5
    t.decimal "chen_F759", precision: 11, scale: 5
    t.decimal "chen_F762", precision: 11, scale: 5
    t.decimal "chen_F770", precision: 11, scale: 5
    t.decimal "chen_F773", precision: 11, scale: 5
    t.decimal "chen_F785", precision: 11, scale: 5
    t.decimal "chen_F786", precision: 11, scale: 5
    t.decimal "chen_F788", precision: 11, scale: 5
    t.decimal "chen_F789", precision: 11, scale: 5
    t.decimal "chen_F790", precision: 11, scale: 5
    t.decimal "chen_F791", precision: 11, scale: 5
    t.decimal "chen_F792", precision: 11, scale: 5
    t.decimal "chen_F793", precision: 11, scale: 5
    t.decimal "chen_F794", precision: 11, scale: 5
    t.decimal "chen_F795", precision: 11, scale: 5
    t.decimal "chen_ala", precision: 11, scale: 5
    t.decimal "chen_epa", precision: 11, scale: 5
    t.decimal "chen_dha", precision: 11, scale: 5
    t.decimal "chen_n3_total", precision: 11, scale: 5
    t.decimal "chen_n3_mean", precision: 11, scale: 5
    t.decimal "chen_VitD", precision: 11, scale: 5
    t.decimal "chen_VitD_mean", precision: 11, scale: 5
    t.decimal "vitK_2_4", precision: 11, scale: 5
    t.decimal "vitK_2_7", precision: 11, scale: 5
    t.decimal "fatty_acids_saturated", precision: 11, scale: 5
  end

  create_table "food_stuff_versions", force: :cascade do |t|
    t.string "filename"
    t.string "url"
    t.integer "org_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
  end

  create_table "food_stuffs", id: :serial, force: :cascade do |t|
    t.string "catalogue"
    t.string "name", null: false
    t.string "english_name"
    t.string "description"
    t.decimal "wastage_rate", precision: 11, scale: 5, default: "-1.0"
    t.decimal "calorie", precision: 11, scale: 5, default: "-1.0"
    t.decimal "water", precision: 11, scale: 5, default: "-1.0"
    t.decimal "protein", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ash", precision: 11, scale: 5, default: "-1.0"
    t.decimal "carbohydrate", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fiber", precision: 11, scale: 5, default: "-1.0"
    t.decimal "dietary_fiber", precision: 11, scale: 5, default: "-1.0"
    t.decimal "total_sugars", precision: 11, scale: 5, default: "-1.0"
    t.decimal "glucose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fructose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "maltose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "sucrose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lactose", precision: 11, scale: 5, default: "-1.0"
    t.decimal "na", precision: 11, scale: 5, default: "-1.0"
    t.decimal "k", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ca", precision: 11, scale: 5, default: "-1.0"
    t.decimal "mg", precision: 11, scale: 5, default: "-1.0"
    t.decimal "p", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fe", precision: 11, scale: 5, default: "-1.0"
    t.decimal "zn", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b1", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b2", precision: 11, scale: 5, default: "-1.0"
    t.decimal "niacin", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b6", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b12", precision: 11, scale: 5, default: "-1.0"
    t.decimal "folicacid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitc", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_te", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vite", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "y_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "g_tocopherols", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_saturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "butyric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "caprylic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "capric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lauric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "tridecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "myristic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pentadecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "palmitic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "margaric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "stearic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "nonadecanoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arachidic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "behenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lignoceric_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_monounsaturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "myristoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "palmitoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "oleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "gadoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "erucic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "linoleic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "fatty_acids_total_polyunsaturated", precision: 11, scale: 5, default: "-1.0"
    t.decimal "linolenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "octadecatetraenoicacid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arachidonic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "eicosapentaenoic_acid_epa", precision: 11, scale: 5, default: "-1.0"
    t.decimal "docosapentaenoic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "docosahexaenoic_acid_dha", precision: 11, scale: 5, default: "-1.0"
    t.string "p_m_s", default: "-1.0"
    t.decimal "cholesterol", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vita_iu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "retinol_equivalentre", precision: 11, scale: 5, default: "-1.0"
    t.decimal "a_carotene", precision: 11, scale: 5, default: "-1.0"
    t.decimal "b_carotene", precision: 11, scale: 5, default: "-1.0"
    t.decimal "retinol", precision: 11, scale: 5, default: "-1.0"
    t.decimal "amino_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "asp", precision: 11, scale: 5, default: "-1.0"
    t.decimal "thr", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ser", precision: 11, scale: 5, default: "-1.0"
    t.decimal "glu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pro", precision: 11, scale: 5, default: "-1.0"
    t.decimal "gly", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ala", precision: 11, scale: 5, default: "-1.0"
    t.decimal "cys", precision: 11, scale: 5, default: "-1.0"
    t.decimal "val", precision: 11, scale: 5, default: "-1.0"
    t.decimal "met", precision: 11, scale: 5, default: "-1.0"
    t.decimal "ile", precision: 11, scale: 5, default: "-1.0"
    t.decimal "leu", precision: 11, scale: 5, default: "-1.0"
    t.decimal "tyr", precision: 11, scale: 5, default: "-1.0"
    t.decimal "phe", precision: 11, scale: 5, default: "-1.0"
    t.decimal "lys", precision: 11, scale: 5, default: "-1.0"
    t.decimal "his", precision: 11, scale: 5, default: "-1.0"
    t.decimal "arg", precision: 11, scale: 5, default: "-1.0"
    t.decimal "trp", precision: 11, scale: 5, default: "-1.0"
    t.decimal "other", precision: 11, scale: 5, default: "-1.0"
    t.decimal "alcohol_content", precision: 11, scale: 5, default: "-1.0"
    t.decimal "revise_calorie", precision: 11, scale: 5, default: "-1.0"
    t.decimal "serving_weight", precision: 11, scale: 5, default: "-1.0"
    t.string "serving_unit"
    t.integer "display_factor", null: false
    t.integer "six_group_factor"
    t.decimal "fruits", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vegetables", precision: 11, scale: 5, default: "-1.0"
    t.decimal "grains", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_low_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_medium_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_high_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "meat_beans_super_high_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_whole_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_low_fat", precision: 11, scale: 5, default: "-1.0"
    t.decimal "milk_skim", precision: 11, scale: 5, default: "-1.0"
    t.decimal "oil", precision: 11, scale: 5, default: "-1.0"
    t.string "source", null: false
    t.string "brand_group"
    t.string "brand"
    t.string "type_or_place"
    t.string "high_fe"
    t.string "low_purine"
    t.string "low_protein"
    t.string "high_mg"
    t.string "high_k"
    t.string "high_ca"
    t.string "high_fiber"
    t.string "non_toxic"
    t.string "organic"
    t.integer "count", default: 0
    t.text "remarks"
    t.string "common_name"
    t.string "low_na"
    t.decimal "ex_weight", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitD", precision: 11, scale: 5, default: "-1.0"
    t.decimal "vitK", precision: 11, scale: 5, default: "-1.0"
    t.decimal "pantothenic_acid", precision: 11, scale: 5, default: "-1.0"
    t.decimal "choline", precision: 11, scale: 5, default: "-1.0"
    t.decimal "biotin", precision: 11, scale: 5, default: "-1.0"
    t.decimal "i", precision: 11, scale: 5, default: "-1.0"
    t.decimal "se", precision: 11, scale: 5, default: "-1.0"
    t.decimal "f", precision: 11, scale: 5, default: "-1.0"
    t.integer "sort"
    t.integer "weight_factor", null: false
    t.decimal "multiply_ratio", precision: 4, scale: 2
    t.string "image_url"
    t.boolean "display", default: true
    t.jsonb "units"
    t.string "catalogue_zh_cn"
    t.string "name_zh_cn"
    t.string "serving_unit_zh_cn"
    t.string "brand_zh_cn"
    t.decimal "tans_fat", precision: 11, scale: 5
    t.decimal "vitD_iu", precision: 11, scale: 5
    t.decimal "vitD_ug", precision: 11, scale: 5
    t.decimal "vitD_2", precision: 11, scale: 5
    t.decimal "vitD_3", precision: 11, scale: 5
    t.decimal "vitK_1", precision: 11, scale: 5
    t.decimal "vitK_2", precision: 11, scale: 5
    t.decimal "cu", precision: 11, scale: 5
    t.decimal "mn", precision: 11, scale: 5
    t.decimal "caproic_acid", precision: 11, scale: 5
    t.decimal "fatty_acids_other", precision: 11, scale: 5
    t.decimal "galactose", precision: 11, scale: 5
    t.decimal "chen_Isfl_Val", precision: 11, scale: 5
    t.decimal "chen_isf710", precision: 11, scale: 5
    t.decimal "chen_isf711", precision: 11, scale: 5
    t.decimal "chen_isf712", precision: 11, scale: 5
    t.decimal "chen_isf713", precision: 11, scale: 5
    t.decimal "chen_isf714", precision: 11, scale: 5
    t.decimal "chen_isf715", precision: 11, scale: 5
    t.decimal "chen_isf716", precision: 11, scale: 5
    t.decimal "chen_F731", precision: 11, scale: 5
    t.decimal "chen_F741", precision: 11, scale: 5
    t.decimal "chen_F742", precision: 11, scale: 5
    t.decimal "chen_F743", precision: 11, scale: 5
    t.decimal "chen_F745", precision: 11, scale: 5
    t.decimal "chen_F746", precision: 11, scale: 5
    t.decimal "chen_F749", precision: 11, scale: 5
    t.decimal "chen_F750", precision: 11, scale: 5
    t.decimal "chen_F751", precision: 11, scale: 5
    t.decimal "chen_F752", precision: 11, scale: 5
    t.decimal "chen_F753", precision: 11, scale: 5
    t.decimal "chen_F755", precision: 11, scale: 5
    t.decimal "chen_F756", precision: 11, scale: 5
    t.decimal "chen_F758", precision: 11, scale: 5
    t.decimal "chen_F759", precision: 11, scale: 5
    t.decimal "chen_F762", precision: 11, scale: 5
    t.decimal "chen_F770", precision: 11, scale: 5
    t.decimal "chen_F773", precision: 11, scale: 5
    t.decimal "chen_F785", precision: 11, scale: 5
    t.decimal "chen_F786", precision: 11, scale: 5
    t.decimal "chen_F788", precision: 11, scale: 5
    t.decimal "chen_F789", precision: 11, scale: 5
    t.decimal "chen_F790", precision: 11, scale: 5
    t.decimal "chen_F791", precision: 11, scale: 5
    t.decimal "chen_F792", precision: 11, scale: 5
    t.decimal "chen_F793", precision: 11, scale: 5
    t.decimal "chen_F794", precision: 11, scale: 5
    t.decimal "chen_F795", precision: 11, scale: 5
    t.decimal "chen_ala", precision: 11, scale: 5
    t.decimal "chen_epa", precision: 11, scale: 5
    t.decimal "chen_dha", precision: 11, scale: 5
    t.decimal "chen_n3_total", precision: 11, scale: 5
    t.decimal "chen_n3_mean", precision: 11, scale: 5
    t.decimal "chen_VitD", precision: 11, scale: 5
    t.decimal "chen_VitD_mean", precision: 11, scale: 5
    t.decimal "vitK_2_4", precision: 11, scale: 5
    t.decimal "vitK_2_7", precision: 11, scale: 5
    t.decimal "fatty_acids_saturated", precision: 11, scale: 5
  end

  create_table "food_tags", id: :serial, force: :cascade do |t|
    t.integer "user_id", null: false
    t.string "food_type", limit: 255, null: false
    t.integer "food_id", null: false
    t.string "tag_type", limit: 255, null: false
    t.integer "tag_id", null: false
    t.boolean "is_black_list", default: false, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["food_id", "food_type"], name: "index_food_tags_on_food_id_and_food_type"
    t.index ["tag_id", "tag_type"], name: "index_food_tags_on_tag_id_and_tag_type"
    t.index ["user_id"], name: "index_food_tags_on_user_id"
  end

  create_table "foods", id: :serial, force: :cascade do |t|
    t.string "name", limit: 255, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.decimal "calorie", default: "-1.0", null: false
    t.decimal "water", default: "-1.0", null: false
    t.decimal "protein", default: "-1.0", null: false
    t.decimal "fat", default: "-1.0", null: false
    t.decimal "ash", default: "-1.0", null: false
    t.decimal "carbohydrate", default: "-1.0", null: false
    t.decimal "dietary_fiber", default: "-1.0", null: false
    t.decimal "sugar", default: "-1.0", null: false
    t.decimal "na", default: "-1.0", null: false
    t.decimal "k", default: "-1.0", null: false
    t.decimal "ca", default: "-1.0", null: false
    t.decimal "mg", default: "-1.0", null: false
    t.decimal "p", default: "-1.0", null: false
    t.decimal "fe", default: "-1.0", null: false
    t.decimal "zn", default: "-1.0", null: false
    t.decimal "vit_b1", default: "-1.0", null: false
    t.decimal "vit_b2", default: "-1.0", null: false
    t.decimal "niacin", default: "-1.0", null: false
    t.decimal "vit_b6", default: "-1.0", null: false
    t.decimal "folic_acid", default: "-1.0", null: false
    t.decimal "vit_c", default: "-1.0", null: false
    t.decimal "alpha_tocopherol_equivalent", default: "-1.0", null: false
    t.decimal "saturated_fatty_acid", default: "-1.0", null: false
    t.decimal "monounsaturated_fatty_acid", default: "-1.0", null: false
    t.decimal "polyunsaturated_fatty_acid", default: "-1.0", null: false
    t.decimal "cholesterol", default: "-1.0", null: false
    t.decimal "vit_a", default: "-1.0", null: false
    t.decimal "retinol_equivalent", default: "-1.0", null: false
    t.decimal "alpha_carotene", default: "-1.0", null: false
    t.decimal "beta_carotene", default: "-1.0", null: false
    t.decimal "other_fatty_acid", default: "-1.0", null: false
    t.decimal "revised_calorie", default: "-1.0", null: false
    t.decimal "vit_e", default: "-1.0", null: false
    t.decimal "hydrolytic_amino_acid", default: "-1.0", null: false
    t.decimal "wasted_rate", default: "-1.0", null: false
    t.decimal "serving_weight", default: "-1.0"
    t.string "p_m_s", limit: 255
    t.string "brand"
    t.string "unit", default: "份"
    t.decimal "cu", default: "-1.0"
    t.decimal "mn", default: "-1.0"
    t.decimal "i", default: "-1.0"
    t.decimal "vit_d", default: "-1.0"
    t.decimal "vit_k", default: "-1.0"
    t.decimal "vit_b12", default: "-1.0"
  end

  create_table "forever_admin_users", id: :serial, force: :cascade do |t|
    t.string "email", default: "", null: false
    t.string "encrypted_password", default: "", null: false
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at"
    t.datetime "remember_created_at"
    t.integer "sign_in_count", default: 0, null: false
    t.datetime "current_sign_in_at"
    t.datetime "last_sign_in_at"
    t.inet "current_sign_in_ip"
    t.inet "last_sign_in_ip"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["email"], name: "index_forever_admin_users_on_email", unique: true
    t.index ["reset_password_token"], name: "index_forever_admin_users_on_reset_password_token", unique: true
  end

  create_table "google_form_forms", id: :serial, force: :cascade do |t|
    t.integer "survey_form_id"
    t.string "google_form_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "google_form_questions", id: :serial, force: :cascade do |t|
    t.integer "google_form_form_id"
    t.integer "question_id"
    t.string "google_form_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "google_form_responses", id: :serial, force: :cascade do |t|
    t.integer "google_form_form_id"
    t.integer "survey_id"
    t.string "google_form_id"
    t.jsonb "answers"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "group_class_activities", id: :serial, force: :cascade do |t|
    t.integer "group_class_id"
    t.string "activity_type"
    t.integer "activity_id"
    t.integer "week"
    t.integer "day"
    t.date "start_date"
    t.datetime "started_at"
    t.string "aasm_state"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "program_topic_title"
    t.string "item_type"
    t.integer "item_id"
    t.datetime "start_date_time"
    t.boolean "enable_announcement"
    t.index ["group_class_id", "item_type", "item_id"], name: "avoiding_duplicate_activity", unique: true
    t.index ["group_class_id"], name: "index_group_class_activities_on_group_class_id"
  end

  create_table "group_class_campaigns", force: :cascade do |t|
    t.string "name"
    t.string "display_name"
    t.bigint "org_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["org_id"], name: "index_group_class_campaigns_on_org_id"
  end

  create_table "group_class_orders", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "message_app_id"
    t.string "aasm_state"
    t.date "paid_at"
    t.date "canceled_at"
    t.integer "client_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "group_class_id"
    t.date "register_at"
    t.string "phone"
    t.string "disease"
    t.string "goal"
    t.date "contacted_at"
    t.date "send_a_payment_link_at"
    t.date "disconnected_at"
    t.integer "pricing_id"
    t.string "registration_code"
    t.date "archived_at"
    t.jsonb "data"
    t.integer "ecpay_order_id"
    t.date "preferred_started_date"
    t.string "payment_method"
    t.integer "care_receiver_id"
    t.integer "kind"
    t.string "plan"
    t.integer "provider", default: 0
    t.string "contract_code"
    t.integer "first_appointment_consulting_dietician_id"
    t.integer "second_appointment_consulting_dietician_id"
    t.datetime "accountable_at"
    t.boolean "is_accounting", default: true
    t.integer "org_id"
    t.integer "group_class_redemption_code_id"
    t.integer "registration_form_coupon_code_id"
    t.integer "referral_code_id"
    t.index ["aasm_state", "group_class_id"], name: "index_group_class_orders_on_aasm_state_and_group_class_id"
    t.index ["client_id"], name: "index_group_class_orders_on_client_id"
    t.index ["group_class_id"], name: "index_group_class_orders_on_group_class_id"
    t.index ["updated_at"], name: "index_group_class_orders_on_updated_at"
  end

  create_table "group_class_redemption_codes", force: :cascade do |t|
    t.string "code"
    t.string "aasm_state"
    t.date "start_at"
    t.date "finish_at"
    t.bigint "group_class_campaign_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["group_class_campaign_id"], name: "index_group_class_redemption_codes_on_group_class_campaign_id"
  end

  create_table "group_class_user_relations", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "group_class_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "role_id"
    t.index ["group_class_id"], name: "index_group_class_user_relations_on_group_class_id"
    t.index ["user_id"], name: "index_group_class_user_relations_on_user_id"
  end

  create_table "group_classes", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.date "started_at"
    t.integer "user_id"
    t.string "aasm_state"
    t.integer "plan_id"
    t.date "finished_at"
    t.string "line_group_link"
    t.integer "satisfaction"
    t.integer "kind"
    t.integer "program_id"
    t.integer "org_id"
    t.jsonb "data"
    t.date "closed_at"
    t.integer "group_class_campaign_id"
    t.boolean "need_service"
  end

  create_table "health_bank_authorizations", force: :cascade do |t|
    t.bigint "client_id"
    t.bigint "user_id"
    t.string "aasm_state"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_health_bank_authorizations_on_client_id"
    t.index ["user_id"], name: "index_health_bank_authorizations_on_user_id"
  end

  create_table "health_banks", force: :cascade do |t|
    t.jsonb "data"
    t.bigint "client_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_health_banks_on_client_id"
  end

  create_table "ingredient_categories", id: :serial, force: :cascade do |t|
    t.string "name", limit: 20, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "ingredients", id: :serial, force: :cascade do |t|
    t.integer "category_id", null: false
    t.string "name", limit: 255, null: false
    t.string "description", limit: 255
    t.decimal "calorie"
    t.decimal "water"
    t.decimal "protein"
    t.decimal "fat"
    t.decimal "ash"
    t.decimal "carbohydrate"
    t.decimal "dietary_fiber"
    t.decimal "sugar"
    t.decimal "na"
    t.decimal "k"
    t.decimal "ca"
    t.decimal "mg"
    t.decimal "p"
    t.decimal "fe"
    t.decimal "zn"
    t.decimal "vit_b1"
    t.decimal "vit_b2"
    t.decimal "niacin"
    t.decimal "vit_b6"
    t.decimal "folic_acid"
    t.decimal "vit_c"
    t.decimal "alpha_tocopherol_equivalent"
    t.decimal "saturated_fatty_acid"
    t.decimal "monounsaturated_fatty_acid"
    t.decimal "polyunsaturated_fatty_acid"
    t.decimal "cholesterol"
    t.decimal "vit_a"
    t.decimal "retinol_equivalent"
    t.decimal "alpha_carotene"
    t.decimal "beta_carotene"
    t.decimal "other_fatty_acid"
    t.decimal "revised_calorie"
    t.string "p_m_s", limit: 255
    t.decimal "vit_e"
    t.decimal "hydrolytic_amino_acid", default: "-1.0", null: false
    t.integer "serving_weight", default: 100, null: false
    t.decimal "wasted_rate", default: "-1.0", null: false
    t.string "food_type"
    t.index ["name"], name: "ix_name", unique: true
  end

  create_table "job_events", id: :serial, force: :cascade do |t|
    t.integer "job_id", null: false
    t.integer "maker_id"
    t.string "maker_type"
    t.string "kind", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["job_id"], name: "index_job_events_on_job_id"
    t.index ["maker_type", "maker_id"], name: "index_job_events_on_maker_type_and_maker_id"
  end

  create_table "jobs", id: :serial, force: :cascade do |t|
    t.integer "requester_id", null: false
    t.integer "assignee_id"
    t.integer "jobable_id", null: false
    t.string "jobable_type", null: false
    t.string "status", null: false
    t.string "kind", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "requester_type", null: false
    t.jsonb "data"
    t.integer "campaign_id"
    t.index ["assignee_id"], name: "index_jobs_on_assignee_id"
    t.index ["campaign_id"], name: "index_jobs_on_campaign_id"
    t.index ["jobable_type", "jobable_id"], name: "index_jobs_on_jobable_type_and_jobable_id"
    t.index ["requester_id", "requester_type"], name: "index_jobs_on_requester_id_and_requester_type"
  end

  create_table "licenses", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.string "name"
    t.string "kind"
    t.jsonb "detail"
    t.string "status"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["user_id"], name: "index_licenses_on_user_id"
  end

  create_table "likes", id: :serial, force: :cascade do |t|
    t.string "record_type"
    t.integer "record_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "sender_id"
    t.string "sender_type"
  end

  create_table "meal_daily_patterns", id: :serial, force: :cascade do |t|
    t.integer "calorie_lower_limit", limit: 2
    t.integer "calorie_upper_limit", limit: 2
    t.integer "carbohydrate_percentage", limit: 2
    t.integer "protein_percentage", limit: 2
    t.integer "fat_percentage", limit: 2
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "meal_dish_recipes", id: :serial, force: :cascade do |t|
    t.integer "dish_id"
    t.integer "food_stuff_id"
    t.decimal "amount", precision: 6, scale: 2
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["dish_id"], name: "index_meal_dish_recipes_on_dish_id"
    t.index ["food_stuff_id"], name: "index_meal_dish_recipes_on_food_stuff_id"
  end

  create_table "meal_dishes", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "kind"
    t.text "text"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "meal_meal_courses", id: :serial, force: :cascade do |t|
    t.integer "meal_id"
    t.integer "dish_id"
    t.integer "kind"
    t.integer "order"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["dish_id"], name: "index_meal_meal_courses_on_dish_id"
    t.index ["meal_id"], name: "index_meal_meal_courses_on_meal_id"
  end

  create_table "meal_patterns", id: :serial, force: :cascade do |t|
    t.integer "daily_pattern_id"
    t.integer "group"
    t.integer "meal_order", limit: 2
    t.integer "calorie"
    t.decimal "fruits", precision: 2, scale: 1
    t.decimal "vegetables", precision: 2, scale: 1
    t.decimal "grains", precision: 2, scale: 1
    t.decimal "meat_beans_low_fat", precision: 2, scale: 1
    t.decimal "meat_beans_medium_fat", precision: 2, scale: 1
    t.decimal "milk_whole_fat", precision: 2, scale: 1
    t.decimal "milk_low_fat", precision: 2, scale: 1
    t.decimal "milk_skim", precision: 2, scale: 1
    t.decimal "oil", precision: 2, scale: 1
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "protein_percentage", limit: 2
    t.integer "carbohydrate_percentage", limit: 2
    t.integer "fat_percentage", limit: 2
    t.decimal "protein", precision: 4, scale: 1
    t.decimal "carbohydrate", precision: 4, scale: 1
    t.decimal "fat", precision: 4, scale: 1
    t.decimal "meat_beans_high_fat", precision: 2, scale: 1
    t.decimal "meat_beans_super_high_fat", precision: 2, scale: 1
    t.index ["daily_pattern_id"], name: "index_meal_patterns_on_daily_pattern_id"
    t.index ["group"], name: "index_meal_patterns_on_group"
    t.index ["meal_order"], name: "index_meal_patterns_on_meal_order"
  end

  create_table "meals", id: :serial, force: :cascade do |t|
    t.string "name"
    t.text "text"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "venue"
  end

  create_table "meals_meal_patterns", id: false, force: :cascade do |t|
    t.integer "meal_id"
    t.integer "meal_pattern_id"
    t.index ["meal_id"], name: "index_meals_meal_patterns_on_meal_id"
    t.index ["meal_pattern_id"], name: "index_meals_meal_patterns_on_meal_pattern_id"
  end

  create_table "measurement_equipment", force: :cascade do |t|
    t.string "name"
    t.string "brand"
    t.string "model"
    t.string "serial"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["serial"], name: "index_measurement_equipment_on_serial"
  end

  create_table "measurements", id: :serial, force: :cascade do |t|
    t.integer "member_id"
    t.string "member_type"
    t.date "date"
    t.datetime "time"
    t.jsonb "data"
    t.string "equip_model"
    t.string "equip_brand"
    t.string "equip_serial"
    t.string "location"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "provider"
    t.string "uid"
    t.index ["date"], name: "index_measurements_on_date"
    t.index ["member_type", "member_id"], name: "index_measurements_on_member_type_and_member_id"
  end

  create_table "message_template_items", id: :serial, force: :cascade do |t|
    t.integer "week"
    t.integer "day"
    t.integer "hour"
    t.integer "minute"
    t.integer "message_template_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "scheduled_message_id"
    t.datetime "time"
    t.boolean "enable_announcement"
    t.index ["message_template_id"], name: "index_message_template_items_on_message_template_id"
  end

  create_table "message_templates", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "receiver_chat_kind"
  end

  create_table "move_rounds", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.integer "session_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_move_rounds_on_client_id"
    t.index ["session_id"], name: "index_move_rounds_on_session_id"
  end

  create_table "move_routines", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.integer "client_id"
    t.integer "user_id"
    t.integer "parent_routine_id"
    t.boolean "is_template", default: false
    t.date "start_date"
    t.string "name"
    t.string "publish_status"
    t.string "status"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_move_routines_on_client_id"
    t.index ["org_id"], name: "index_move_routines_on_org_id"
    t.index ["parent_routine_id"], name: "index_move_routines_on_parent_routine_id"
    t.index ["user_id"], name: "index_move_routines_on_user_id"
  end

  create_table "move_session_items", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.integer "round_id"
    t.integer "item_id"
    t.integer "duration"
    t.decimal "distance", precision: 5, scale: 1
    t.decimal "weight", precision: 5, scale: 1
    t.integer "sets", limit: 2
    t.integer "reps", limit: 2
    t.boolean "each_side", default: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "session_id"
    t.jsonb "data"
    t.index ["client_id"], name: "index_move_session_items_on_client_id"
    t.index ["item_id"], name: "index_move_session_items_on_item_id"
    t.index ["round_id"], name: "index_move_session_items_on_round_id"
    t.index ["session_id"], name: "index_move_session_items_on_session_id"
  end

  create_table "move_sessions", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.integer "routine_id"
    t.integer "parent_session_id"
    t.boolean "is_template", default: false
    t.string "name"
    t.date "date"
    t.time "starts_at"
    t.string "publish_status"
    t.string "status"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "note_id"
    t.string "remark"
    t.decimal "calorie", precision: 5, scale: 1
    t.index ["client_id"], name: "index_move_sessions_on_client_id"
    t.index ["note_id"], name: "index_move_sessions_on_note_id"
    t.index ["parent_session_id"], name: "index_move_sessions_on_parent_session_id"
    t.index ["routine_id"], name: "index_move_sessions_on_routine_id"
  end

  create_table "nestle_admin_users", id: :serial, force: :cascade do |t|
    t.string "email", default: "", null: false
    t.string "encrypted_password", default: "", null: false
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at"
    t.datetime "remember_created_at"
    t.integer "sign_in_count", default: 0, null: false
    t.datetime "current_sign_in_at"
    t.datetime "last_sign_in_at"
    t.inet "current_sign_in_ip"
    t.inet "last_sign_in_ip"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["email"], name: "index_nestle_admin_users_on_email", unique: true
    t.index ["reset_password_token"], name: "index_nestle_admin_users_on_reset_password_token", unique: true
  end

  create_table "note_assets", id: :serial, force: :cascade do |t|
    t.integer "note_id"
    t.integer "client_id"
    t.string "bucket"
    t.string "key"
    t.string "url"
    t.text "text"
    t.jsonb "data"
    t.date "date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "comments_count", default: 0
    t.string "kind"
    t.integer "items_count", default: 0
    t.string "item_names"
    t.integer "member_id"
    t.string "member_type"
    t.integer "user_id"
    t.string "media_type"
    t.string "uid"
    t.index ["client_id"], name: "index_note_assets_on_client_id"
    t.index ["key"], name: "index_note_assets_on_key"
    t.index ["media_type", "key"], name: "index_note_assets_on_media_type_and_key"
    t.index ["member_type", "member_id"], name: "index_note_assets_on_member_type_and_member_id"
    t.index ["note_id"], name: "index_note_assets_on_note_id"
    t.index ["uid"], name: "index_note_assets_on_uid"
    t.index ["user_id"], name: "index_note_assets_on_user_id"
  end

  create_table "note_items", id: :serial, force: :cascade do |t|
    t.integer "note_id"
    t.date "date"
    t.jsonb "data"
    t.integer "item_id"
    t.string "item_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "note_asset_id"
    t.integer "client_id"
    t.integer "member_id"
    t.string "member_type"
    t.integer "user_id"
    t.index ["member_type", "member_id"], name: "index_note_items_on_member_type_and_member_id"
    t.index ["note_asset_id"], name: "index_note_items_on_note_asset_id"
    t.index ["note_id"], name: "index_note_items_on_note_id"
    t.index ["user_id"], name: "index_note_items_on_user_id"
  end

  create_table "note_places", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "vicinity"
    t.string "provider"
    t.string "place_id"
    t.decimal "longitude", precision: 10, scale: 7
    t.decimal "latitude", precision: 10, scale: 7
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "notes", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.string "type"
    t.date "date"
    t.jsonb "data"
    t.text "text"
    t.integer "note_items_count"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "comments_count", default: 0
    t.boolean "user_replied", default: false
    t.integer "member_id"
    t.string "member_type"
    t.integer "user_id"
    t.integer "place_id"
    t.integer "creater_id"
    t.string "creater_type"
    t.index ["client_id"], name: "index_notes_on_client_id"
    t.index ["creater_type", "creater_id"], name: "index_notes_on_creater_type_and_creater_id"
    t.index ["date", "client_id"], name: "index_notes_on_date_and_client_id"
    t.index ["date"], name: "index_notes_on_date"
    t.index ["member_type", "member_id"], name: "index_notes_on_member_type_and_member_id"
    t.index ["user_id"], name: "index_notes_on_user_id"
  end

  create_table "notifications", id: :serial, force: :cascade do |t|
    t.integer "recipient_id"
    t.string "category"
    t.string "title"
    t.text "content"
    t.boolean "read", default: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "record_type"
    t.integer "record_id"
    t.integer "sender_id"
    t.jsonb "data"
    t.string "sender_type"
    t.string "recipient_type"
    t.integer "batch_action_history_id"
    t.index ["read", "recipient_id", "recipient_type"], name: "index_notifications_on_recipient_and_read"
    t.index ["recipient_id", "recipient_type", "created_at"], name: "index_notifications_on_recipient_and_created_at"
    t.index ["recipient_id", "recipient_type"], name: "index_notifications_on_recipient_id_and_recipient_type"
    t.index ["record_id", "record_type"], name: "index_notifications_on_record_id_and_record_type"
    t.index ["sender_id", "sender_type"], name: "index_notifications_on_sender_id_and_sender_type"
  end

  create_table "notings", id: :serial, force: :cascade do |t|
    t.integer "note_id"
    t.integer "noteable_id"
    t.string "noteable_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "org_client_profiles", force: :cascade do |t|
    t.bigint "client_id"
    t.bigint "org_id"
    t.string "record_number"
    t.string "identity_number"
    t.string "gender"
    t.date "birthday"
    t.string "address"
    t.string "mobile"
    t.string "phone_number"
    t.string "email"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "real_name"
    t.integer "branch_id"
    t.index ["birthday"], name: "index_org_client_profiles_on_birthday"
    t.index ["client_id"], name: "index_org_client_profiles_on_client_id"
    t.index ["org_id"], name: "index_org_client_profiles_on_org_id"
  end

  create_table "org_client_scores", id: :serial, force: :cascade do |t|
    t.integer "group_id"
    t.integer "client_id"
    t.decimal "points", precision: 4, scale: 1, default: "0.0"
    t.string "dur", default: "day"
    t.jsonb "data"
    t.date "start_date"
    t.date "end_date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_org_client_scores_on_client_id"
    t.index ["group_id"], name: "index_org_client_scores_on_group_id"
  end

  create_table "org_contest_scores", id: :serial, force: :cascade do |t|
    t.integer "contest_id"
    t.integer "group_id"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["contest_id"], name: "index_org_contest_scores_on_contest_id"
    t.index ["group_id"], name: "index_org_contest_scores_on_group_id"
  end

  create_table "org_contests", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.string "name", null: false
    t.text "description"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.date "start_date"
    t.date "end_date"
    t.jsonb "data"
    t.index ["org_id"], name: "index_org_contests_on_org_id"
  end

  create_table "org_group_memberships", id: :serial, force: :cascade do |t|
    t.integer "group_id"
    t.integer "member_id"
    t.string "member_type"
    t.string "uid"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["group_id"], name: "index_org_group_memberships_on_group_id"
    t.index ["member_type", "member_id"], name: "index_org_group_memberships_on_member_type_and_member_id"
  end

  create_table "org_groups", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.string "name", null: false
    t.string "kind"
    t.text "description"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
  end

  create_table "org_groups_clients", id: false, force: :cascade do |t|
    t.integer "group_id"
    t.integer "client_id"
    t.index ["group_id", "client_id"], name: "index_org_groups_clients_on_group_id_and_client_id", unique: true
  end

  create_table "org_groups_contests", id: false, force: :cascade do |t|
    t.integer "group_id"
    t.integer "contest_id"
  end

  create_table "org_groups_users", id: false, force: :cascade do |t|
    t.integer "group_id"
    t.integer "user_id"
    t.index ["group_id", "user_id"], name: "index_org_groups_users_on_group_id_and_user_id", unique: true
  end

  create_table "org_memberships", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.integer "member_id"
    t.string "member_type"
    t.string "uid"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["member_type", "member_id"], name: "index_org_memberships_on_member_type_and_member_id"
    t.index ["org_id"], name: "index_org_memberships_on_org_id"
  end

  create_table "org_referral_set_relations", force: :cascade do |t|
    t.bigint "org_id"
    t.bigint "referral_set_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["org_id"], name: "index_org_referral_set_relations_on_org_id"
    t.index ["referral_set_id"], name: "index_org_referral_set_relations_on_referral_set_id"
  end

  create_table "orgs", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.string "kind"
    t.text "description"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "display_name"
    t.string "avatar_url"
    t.jsonb "data"
    t.string "api_key"
    t.boolean "is_default", default: false
    t.index ["name"], name: "index_orgs_on_name", unique: true
  end

  create_table "orgs_admins", id: false, force: :cascade do |t|
    t.integer "org_id"
    t.integer "user_id"
    t.index ["org_id", "user_id"], name: "index_orgs_admins_on_org_id_and_user_id", unique: true
  end

  create_table "orgs_clients", id: false, force: :cascade do |t|
    t.integer "org_id"
    t.integer "client_id"
    t.index ["org_id", "client_id"], name: "index_orgs_clients_on_org_id_and_client_id", unique: true
  end

  create_table "orgs_users", id: false, force: :cascade do |t|
    t.integer "org_id"
    t.integer "user_id"
    t.index ["org_id", "user_id"], name: "index_orgs_users_on_org_id_and_user_id", unique: true
  end

  create_table "personal_ingredients", id: :serial, force: :cascade do |t|
    t.integer "user_id", null: false
    t.integer "category_id", null: false
    t.string "name", limit: 255, null: false
    t.string "description", limit: 255
    t.decimal "calorie", default: "-1.0"
    t.decimal "water", default: "-1.0"
    t.decimal "protein", default: "-1.0"
    t.decimal "fat", default: "-1.0"
    t.decimal "ash", default: "-1.0"
    t.decimal "carbohydrate", default: "-1.0"
    t.decimal "dietary_fiber", default: "-1.0"
    t.decimal "sugar", default: "-1.0"
    t.decimal "na", default: "-1.0"
    t.decimal "k", default: "-1.0"
    t.decimal "ca", default: "-1.0"
    t.decimal "mg", default: "-1.0"
    t.decimal "p", default: "-1.0"
    t.decimal "fe", default: "-1.0"
    t.decimal "zn", default: "-1.0"
    t.decimal "vit_b1", default: "-1.0"
    t.decimal "vit_b2", default: "-1.0"
    t.decimal "niacin", default: "-1.0"
    t.decimal "vit_b6", default: "-1.0"
    t.decimal "folic_acid", default: "-1.0"
    t.decimal "vit_c", default: "-1.0"
    t.decimal "alpha_tocopherol_equivalent", default: "-1.0"
    t.decimal "saturated_fatty_acid", default: "-1.0"
    t.decimal "monounsaturated_fatty_acid", default: "-1.0"
    t.decimal "polyunsaturated_fatty_acid", default: "-1.0"
    t.decimal "cholesterol", default: "-1.0"
    t.decimal "vit_a", default: "-1.0"
    t.decimal "retinol_equivalent", default: "-1.0"
    t.decimal "alpha_carotene", default: "-1.0"
    t.decimal "beta_carotene", default: "-1.0"
    t.decimal "other_fatty_acid", default: "-1.0"
    t.decimal "revised_calorie", default: "-1.0"
    t.string "p_m_s", limit: 255
    t.decimal "vit_e", default: "-1.0"
    t.decimal "hydrolytic_amino_acid", default: "-1.0", null: false
    t.integer "serving_weight", default: 100, null: false
    t.decimal "wasted_rate", default: "-1.0", null: false
    t.index ["user_id", "name"], name: "personal_ingredients_ix_user_id_name"
  end

  create_table "personal_plans", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.string "display_name", null: false
    t.text "features"
    t.decimal "amount", precision: 10, scale: 2, null: false
    t.string "currency", default: "NTD", null: false
    t.string "interval"
    t.integer "interval_count"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.boolean "recurring"
  end

  create_table "personal_pricings", id: :serial, force: :cascade do |t|
    t.integer "plan_id"
    t.string "name", null: false
    t.string "display_name", null: false
    t.decimal "amount", precision: 10, scale: 2, null: false
    t.text "features"
    t.boolean "discount"
    t.string "discount_text"
    t.string "highlight_text"
    t.string "action_text"
    t.string "currency", default: "NTD", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "cover_photo"
    t.jsonb "detail"
    t.string "cover_photo_url"
    t.string "cover_photo_title"
    t.string "cover_photo_subtitle"
    t.string "content_url"
    t.integer "realm_id"
    t.integer "org_id"
    t.index ["name"], name: "index_personal_pricings_on_name"
    t.index ["plan_id"], name: "index_personal_pricings_on_plan_id"
  end

  create_table "personal_subscriptions", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "pricing_id"
    t.string "number"
    t.string "status", null: false
    t.boolean "recurring"
    t.string "handler"
    t.string "interval"
    t.integer "interval_count"
    t.integer "credit_card_exec_times"
    t.decimal "amount", precision: 10, scale: 2, null: false
    t.string "currency", default: "NTD", null: false
    t.string "payment_type"
    t.date "starts_on"
    t.date "ends_on"
    t.date "current_period_starts_on"
    t.date "current_period_ends_on"
    t.string "verify_number"
    t.string "tax_id"
    t.integer "invoice_type"
    t.string "invoice_title"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "client_id"
    t.index ["pricing_id"], name: "index_personal_subscriptions_on_pricing_id"
    t.index ["user_id"], name: "index_personal_subscriptions_on_user_id"
  end

  create_table "personal_tags", id: :serial, force: :cascade do |t|
    t.integer "user_id", null: false
    t.string "name", limit: 255, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["user_id"], name: "index_personal_tags_on_user_id"
  end

  create_table "pharmacy_medicine_preset_groups", force: :cascade do |t|
    t.bigint "branch_id"
    t.string "code"
    t.string "name"
    t.string "status"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["branch_id"], name: "index_pharmacy_medicine_preset_groups_on_branch_id"
    t.index ["code"], name: "index_pharmacy_medicine_preset_groups_on_code"
    t.index ["status"], name: "index_pharmacy_medicine_preset_groups_on_status"
  end

  create_table "pharmacy_medicine_presets", force: :cascade do |t|
    t.bigint "preset_group_id"
    t.bigint "medicine_id"
    t.decimal "single_amount", precision: 6, scale: 2
    t.string "frequency"
    t.decimal "frequency_ratio", precision: 6, scale: 4
    t.integer "days"
    t.text "basic_compliance"
    t.string "status"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["medicine_id"], name: "index_pharmacy_medicine_presets_on_medicine_id"
    t.index ["preset_group_id"], name: "index_pharmacy_medicine_presets_on_preset_group_id"
    t.index ["status"], name: "index_pharmacy_medicine_presets_on_status"
  end

  create_table "pharmacy_medicines", force: :cascade do |t|
    t.bigint "branch_id"
    t.string "status"
    t.string "code"
    t.string "code_color"
    t.string "english_name"
    t.string "chinese_name"
    t.string "generic_name"
    t.string "usage"
    t.string "route"
    t.string "minimum_unit"
    t.string "package_unit"
    t.integer "package_amount"
    t.integer "safe_stock_amount"
    t.string "side_effect"
    t.text "notice"
    t.integer "self_pay_fee"
    t.integer "expiration_alert_days"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["branch_id"], name: "index_pharmacy_medicines_on_branch_id"
    t.index ["code"], name: "index_pharmacy_medicines_on_code"
    t.index ["status"], name: "index_pharmacy_medicines_on_status"
  end

  create_table "pharmacy_purchase_medicine_orders", force: :cascade do |t|
    t.bigint "medicine_id"
    t.bigint "pharmacy_purchase_id"
    t.integer "amount"
    t.integer "subtotal"
    t.decimal "unit_price", precision: 9, scale: 3
    t.text "note"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.date "expiration_date"
    t.text "batch_number"
    t.index ["medicine_id"], name: "index_pharmacy_purchase_medicine_orders_on_medicine_id"
    t.index ["pharmacy_purchase_id"], name: "index_pharmacy_purchase_medicine_orders_on_pharmacy_purchase_id"
  end

  create_table "pharmacy_purchases", force: :cascade do |t|
    t.bigint "user_id"
    t.bigint "org_id"
    t.bigint "branch_id"
    t.bigint "supplier_id"
    t.text "supplier_name"
    t.text "supplier_phone"
    t.text "order_number"
    t.date "order_date"
    t.integer "sum"
    t.text "note"
    t.text "serial_number"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.text "invoice_number"
    t.integer "invoice_price"
    t.text "shipping_number"
    t.date "shipping_date"
  end

  create_table "pharmacy_stock_bookings", force: :cascade do |t|
    t.bigint "medicine_id"
    t.string "source_type"
    t.bigint "source_id"
    t.decimal "amount", precision: 8, scale: 2, default: "0.0"
    t.date "date"
    t.datetime "datetime"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["date"], name: "index_pharmacy_stock_bookings_on_date"
    t.index ["medicine_id"], name: "index_pharmacy_stock_bookings_on_medicine_id"
    t.index ["source_type", "source_id"], name: "index_pharmacy_stock_bookings_on_source_type_and_source_id"
  end

  create_table "pharmacy_stock_incomings", force: :cascade do |t|
    t.bigint "medicine_id"
    t.bigint "order_id"
    t.string "source_type"
    t.bigint "source_id"
    t.decimal "amount", precision: 8, scale: 2, default: "0.0"
    t.date "expiration_date"
    t.date "date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["date"], name: "index_pharmacy_stock_incomings_on_date"
    t.index ["medicine_id"], name: "index_pharmacy_stock_incomings_on_medicine_id"
    t.index ["order_id"], name: "index_pharmacy_stock_incomings_on_order_id"
    t.index ["source_type", "source_id"], name: "index_pharmacy_stock_incomings_on_source_type_and_source_id"
  end

  create_table "pharmacy_stock_input_orders", force: :cascade do |t|
    t.bigint "user_id"
    t.bigint "org_id"
    t.bigint "branch_id"
    t.string "source_order_type"
    t.bigint "source_order_id"
    t.string "serial_number"
    t.date "date"
    t.datetime "datetime"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["branch_id", "date"], name: "index_input_orders_on_branch_and_date"
    t.index ["branch_id", "serial_number"], name: "index_input_orders_on_branch_and_sn"
    t.index ["branch_id"], name: "index_pharmacy_stock_input_orders_on_branch_id"
    t.index ["date"], name: "index_pharmacy_stock_input_orders_on_date"
    t.index ["org_id"], name: "index_pharmacy_stock_input_orders_on_org_id"
    t.index ["source_order_type", "source_order_id"], name: "index_stock_input_orders_on_source_order"
    t.index ["user_id"], name: "index_pharmacy_stock_input_orders_on_user_id"
  end

  create_table "pharmacy_stock_outgoings", force: :cascade do |t|
    t.bigint "medicine_id"
    t.bigint "order_id"
    t.string "source_type"
    t.bigint "source_id"
    t.decimal "amount", precision: 8, scale: 2, default: "0.0"
    t.date "date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "returned", default: false
    t.string "return_note"
    t.bigint "incoming_id"
    t.index ["date"], name: "index_pharmacy_stock_outgoings_on_date"
    t.index ["incoming_id"], name: "index_pharmacy_stock_outgoings_on_incoming_id"
    t.index ["medicine_id"], name: "index_pharmacy_stock_outgoings_on_medicine_id"
    t.index ["order_id"], name: "index_pharmacy_stock_outgoings_on_order_id"
    t.index ["source_type", "source_id"], name: "index_pharmacy_stock_outgoings_on_source_type_and_source_id"
  end

  create_table "pharmacy_stock_output_orders", force: :cascade do |t|
    t.bigint "user_id"
    t.bigint "org_id"
    t.bigint "branch_id"
    t.string "source_order_type"
    t.bigint "source_order_id"
    t.string "serial_number"
    t.date "date"
    t.datetime "datetime"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "returned", default: false
    t.index ["branch_id", "date"], name: "index_output_orders_on_branch_and_date"
    t.index ["branch_id", "serial_number"], name: "index_output_orders_on_branch_and_sn"
    t.index ["branch_id"], name: "index_pharmacy_stock_output_orders_on_branch_id"
    t.index ["date"], name: "index_pharmacy_stock_output_orders_on_date"
    t.index ["org_id"], name: "index_pharmacy_stock_output_orders_on_org_id"
    t.index ["source_order_type", "source_order_id"], name: "index_stock_output_orders_on_source_order"
    t.index ["user_id"], name: "index_pharmacy_stock_output_orders_on_user_id"
  end

  create_table "pharmacy_stock_snapshots", force: :cascade do |t|
    t.bigint "medicine_id"
    t.date "date"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["date"], name: "index_pharmacy_stock_snapshots_on_date"
    t.index ["medicine_id", "date"], name: "index_pharmacy_stock_snapshots_on_medicine_id_and_date"
  end

  create_table "pharmacy_stocks", force: :cascade do |t|
    t.bigint "medicine_id"
    t.decimal "balance_amount", precision: 8, scale: 2, default: "0.0"
    t.decimal "booked_amount", precision: 8, scale: 2, default: "0.0"
    t.decimal "outputable_amount", precision: 8, scale: 2, default: "0.0"
    t.date "expiration_date"
    t.decimal "expiring_amount", precision: 8, scale: 2, default: "0.0"
    t.date "last_incoming_date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "branch_id"
    t.index ["expiration_date"], name: "index_pharmacy_stocks_on_expiration_date"
    t.index ["medicine_id"], name: "index_pharmacy_stocks_on_medicine_id"
  end

  create_table "pharmacy_suppliers", force: :cascade do |t|
    t.text "name"
    t.jsonb "data", default: {}
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.bigint "branch_id"
  end

  create_table "position_tags", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.text "content"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "positions", id: :serial, force: :cascade do |t|
    t.string "category", null: false
    t.string "name", null: false
    t.string "reference"
    t.text "tips"
    t.string "english_name"
    t.string "naming"
  end

  create_table "post_keeps", id: :serial, force: :cascade do |t|
    t.integer "post_id"
    t.integer "sender_id"
    t.string "sender_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["post_id"], name: "index_post_keeps_on_post_id"
    t.index ["sender_type", "sender_id"], name: "index_post_keeps_on_sender_type_and_sender_id"
  end

  create_table "post_likes", id: :serial, force: :cascade do |t|
    t.integer "post_id"
    t.integer "sender_id"
    t.string "sender_type"
    t.datetime "created_at", null: false
    t.index ["post_id", "sender_id", "sender_type"], name: "index_post_likes_on_post_id_and_sender_id_and_sender_type", unique: true
    t.index ["post_id"], name: "index_post_likes_on_post_id"
    t.index ["sender_type", "sender_id"], name: "index_post_likes_on_sender_type_and_sender_id"
  end

  create_table "post_moderations", id: :serial, force: :cascade do |t|
    t.integer "post_id"
    t.boolean "pass", default: false
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["post_id"], name: "index_post_moderations_on_post_id"
  end

  create_table "post_shares", id: :serial, force: :cascade do |t|
    t.integer "post_id"
    t.integer "new_post_id"
    t.integer "sender_id"
    t.string "sender_type"
    t.integer "target_id"
    t.string "target_type"
    t.datetime "created_at", null: false
    t.index ["new_post_id"], name: "index_post_shares_on_new_post_id"
    t.index ["post_id"], name: "index_post_shares_on_post_id"
    t.index ["sender_type", "sender_id"], name: "index_post_shares_on_sender_type_and_sender_id"
    t.index ["target_type", "target_id"], name: "index_post_shares_on_target_type_and_target_id"
  end

  create_table "post_shows", id: :serial, force: :cascade do |t|
    t.integer "post_id"
    t.integer "sender_id"
    t.string "sender_type"
    t.datetime "created_at", null: false
    t.index ["post_id", "sender_id", "sender_type"], name: "index_post_shows_on_post_id_and_sender_id_and_sender_type", unique: true
    t.index ["post_id"], name: "index_post_shows_on_post_id"
    t.index ["sender_type", "sender_id"], name: "index_post_shows_on_sender_type_and_sender_id"
  end

  create_table "post_views", id: :serial, force: :cascade do |t|
    t.integer "post_id"
    t.integer "sender_id"
    t.string "sender_type"
    t.datetime "created_at", null: false
    t.index ["post_id", "sender_id", "sender_type"], name: "index_post_views_on_post_id_and_sender_id_and_sender_type", unique: true
    t.index ["post_id"], name: "index_post_views_on_post_id"
    t.index ["sender_type", "sender_id"], name: "index_post_views_on_sender_type_and_sender_id"
  end

  create_table "posts", id: :serial, force: :cascade do |t|
    t.integer "author_id"
    t.string "author_type"
    t.integer "object_id"
    t.string "object_type"
    t.integer "target_id"
    t.string "target_type"
    t.integer "original_post_id"
    t.jsonb "data"
    t.integer "likes_count", default: 0
    t.integer "shares_count", default: 0
    t.integer "comments_count", default: 0
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.text "content"
    t.datetime "published_at"
    t.boolean "hidden", default: false
    t.bigint "hot_score", default: 0
    t.integer "shows_count", default: 0
    t.integer "views_count", default: 0
    t.integer "realm_id"
    t.string "aasm_state"
    t.string "moderation_state"
    t.boolean "public"
    t.integer "keeps_count", default: 0
    t.bigint "like_score", default: 0
    t.bigint "comment_score", default: 0
    t.integer "sticky", default: 0
    t.index ["author_type", "author_id"], name: "index_posts_on_author_type_and_author_id"
    t.index ["hot_score"], name: "index_posts_on_hot_score"
    t.index ["object_type", "object_id"], name: "index_posts_on_object_type_and_object_id"
    t.index ["original_post_id"], name: "index_posts_on_original_post_id"
    t.index ["public"], name: "index_posts_on_public"
    t.index ["realm_id"], name: "index_posts_on_realm_id"
    t.index ["target_type", "target_id"], name: "index_posts_on_target_type_and_target_id"
  end

  create_table "pq_store_credit_issuances", force: :cascade do |t|
    t.bigint "client_id"
    t.bigint "app_id"
    t.string "pq_store_id"
    t.string "op_id"
    t.integer "state"
    t.string "op_name"
    t.string "name"
    t.string "display_name"
    t.datetime "scheduled_at"
    t.datetime "issued_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.integer "pq_integral_id"
    t.integer "pq_integral_amount"
    t.index ["app_id"], name: "index_pq_store_credit_issuances_on_app_id"
    t.index ["client_id"], name: "index_pq_store_credit_issuances_on_client_id"
  end

  create_table "pq_store_user_identities", force: :cascade do |t|
    t.bigint "user_id"
    t.bigint "app_id"
    t.string "mobile"
    t.string "coupon_code"
    t.boolean "saved", default: false
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["app_id"], name: "index_pq_store_user_identities_on_app_id"
    t.index ["user_id"], name: "index_pq_store_user_identities_on_user_id"
  end

  create_table "product_sku_listings", id: :serial, force: :cascade do |t|
    t.integer "product_id"
    t.integer "product_sku_pricing_id"
    t.integer "order"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "amount", default: 1
  end

  create_table "product_sku_pricings", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.integer "product_sku_id"
    t.string "name"
    t.string "display_name"
    t.text "description"
    t.integer "price"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "product_skus", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.string "name"
    t.string "display_name"
    t.string "unit"
    t.text "description"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.text "spec_text"
    t.integer "kind"
  end

  create_table "products", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.string "code"
    t.string "name"
    t.string "display_name"
    t.text "description"
    t.integer "list_price"
    t.integer "price"
    t.string "unit"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "kind"
  end

  create_table "program_course_set_template_relationships", id: :serial, force: :cascade do |t|
    t.integer "program_id"
    t.integer "course_set_template_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "program_message_template_relationships", id: :serial, force: :cascade do |t|
    t.integer "program_id"
    t.integer "message_template_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "program_topics", id: :serial, force: :cascade do |t|
    t.string "title"
    t.integer "week"
    t.integer "program_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["program_id"], name: "index_program_topics_on_program_id"
  end

  create_table "programs", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "avatar_url"
    t.integer "org_id"
    t.jsonb "data"
  end

  create_table "qr_sessions", id: :uuid, default: -> { "uuid_generate_v4()" }, force: :cascade do |t|
    t.string "device_session_id"
    t.string "secret"
    t.string "state"
    t.integer "app_id"
    t.integer "member_id"
    t.string "member_type"
    t.datetime "expires_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "question_sets", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "survey_form_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "questions_count"
    t.text "description"
    t.bigint "parent_id"
    t.integer "score_calculation_type"
    t.jsonb "data", default: {}
    t.index ["parent_id"], name: "index_question_sets_on_parent_id"
    t.index ["survey_form_id"], name: "index_question_sets_on_survey_form_id"
  end

  create_table "questions", id: :serial, force: :cascade do |t|
    t.string "title"
    t.integer "kind"
    t.integer "order"
    t.jsonb "data"
    t.integer "completes_count", default: 0
    t.integer "course_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "source_type"
    t.integer "source_id"
    t.boolean "is_must"
    t.string "sub_condition"
    t.integer "max_choice"
    t.integer "question_id"
    t.boolean "hidden_from_app", default: false
    t.bigint "question_set_id"
    t.index ["course_id"], name: "index_questions_on_course_id"
    t.index ["question_set_id"], name: "index_questions_on_question_set_id"
  end

  create_table "quick_adds", id: :serial, force: :cascade do |t|
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "realms", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "display_name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "recipe_ingredients", id: :serial, force: :cascade do |t|
    t.integer "recipe_id", null: false
    t.integer "ingredient_id", null: false
    t.decimal "amount", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["ingredient_id"], name: "index_recipe_ingredients_on_ingredient_id"
    t.index ["recipe_id"], name: "index_recipe_ingredients_on_recipe_id"
  end

  create_table "recipes", id: :serial, force: :cascade do |t|
    t.string "name", limit: 255, null: false
    t.integer "user_id", null: false
    t.string "brand", limit: 255, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["user_id"], name: "index_recipes_on_user_id"
  end

  create_table "referral_code_referral_set_relations", force: :cascade do |t|
    t.bigint "referral_code_id"
    t.bigint "referral_set_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["referral_code_id"], name: "index_referral_code_referral_set_relations_on_referral_code_id"
    t.index ["referral_set_id"], name: "index_referral_code_referral_set_relations_on_referral_set_id"
  end

  create_table "referral_codes", force: :cascade do |t|
    t.string "name"
    t.string "code"
    t.string "member_type"
    t.integer "member_id"
    t.string "aasm_state"
    t.date "start_at"
    t.date "finish_at"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "referral_set_items", force: :cascade do |t|
    t.integer "point", null: false
    t.bigint "referral_set_id"
    t.bigint "registration_form_plan_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["referral_set_id"], name: "index_referral_set_items_on_referral_set_id"
    t.index ["registration_form_plan_id"], name: "index_referral_set_items_on_registration_form_plan_id"
  end

  create_table "referral_sets", force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "default", default: false
  end

  create_table "registration_form_assistant_relationships", id: :serial, force: :cascade do |t|
    t.integer "registration_form_plan_id"
    t.integer "user_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "registration_form_coupon_code_ecpay_order_relationships", id: :serial, force: :cascade do |t|
    t.integer "registration_form_coupon_code_id"
    t.integer "ecpay_order_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "registration_form_coupon_codes", id: :serial, force: :cascade do |t|
    t.integer "registration_form_id"
    t.string "name"
    t.integer "point", default: 0, null: false
    t.integer "quantity", default: 0, null: false
    t.string "code"
    t.date "started_at"
    t.date "finished_at"
    t.string "aasm_state"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["registration_form_id"], name: "index_registration_form_coupon_codes_on_registration_form_id"
  end

  create_table "registration_form_hypeman_relationships", id: :serial, force: :cascade do |t|
    t.integer "registration_form_plan_id"
    t.integer "user_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "registration_form_master_dietician_relationships", id: :serial, force: :cascade do |t|
    t.integer "registration_form_plan_id"
    t.integer "user_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "registration_form_plan_user_relations", force: :cascade do |t|
    t.bigint "role_id"
    t.bigint "registration_form_plan_id"
    t.bigint "user_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "avatar_url"
    t.index ["registration_form_plan_id"], name: "registration_form_plan_user_relation_registration_form_plan"
    t.index ["role_id"], name: "registration_form_plan_user_relation_role"
    t.index ["user_id"], name: "registration_form_plan_user_relation_user"
  end

  create_table "registration_form_plans", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "original_price"
    t.integer "special_price"
    t.integer "registration_form_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "kind"
    t.boolean "auto_create_group_class"
    t.integer "user_id"
    t.integer "program_id"
    t.integer "course_length_in_days"
    t.integer "course_start_week"
    t.integer "course_start_week_day"
    t.integer "course_start_days"
    t.integer "redeem_limit", default: 0
    t.boolean "default"
    t.string "dietician_avatar_url"
    t.index ["registration_form_id"], name: "index_registration_form_plans_on_registration_form_id"
  end

  create_table "registration_form_questions", id: :serial, force: :cascade do |t|
    t.integer "registration_form_id"
    t.string "name"
    t.boolean "displayed", default: false, null: false
    t.boolean "required", default: false, null: false
    t.string "label"
    t.string "placeholder"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["registration_form_id"], name: "index_registration_form_questions_on_registration_form_id"
  end

  create_table "registration_form_reminders", id: :serial, force: :cascade do |t|
    t.string "content"
    t.boolean "emphasize", default: false, null: false
    t.integer "registration_form_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["registration_form_id"], name: "index_registration_form_reminders_on_registration_form_id"
  end

  create_table "registration_form_suitable_started_dates", id: :serial, force: :cascade do |t|
    t.date "suitable_started_date"
    t.integer "registration_form_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "aasm_state"
  end

  create_table "registration_form_titles", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "registration_form_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["registration_form_id"], name: "index_registration_form_titles_on_registration_form_id"
  end

  create_table "registration_forms", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "banner_file_name"
    t.string "banner_content_type"
    t.integer "banner_file_size"
    t.datetime "banner_updated_at"
    t.string "trade_desc"
    t.boolean "payment_method_credit_card", default: false, null: false
    t.boolean "payment_method_atm", default: false, null: false
    t.boolean "payment_method_web_atm", default: false, null: false
    t.boolean "payment_method_csv", default: false, null: false
    t.boolean "group_class_order_name_field", default: false, null: false
    t.boolean "group_class_order_name_required", default: false, null: false
    t.boolean "group_class_order_cellphone_number_field", default: false, null: false
    t.boolean "group_class_order_cellphone_number_required", default: false, null: false
    t.boolean "group_class_order_address_field", default: false, null: false
    t.boolean "group_class_order_address_required", default: false, null: false
    t.boolean "group_class_order_email_field", default: false, null: false
    t.boolean "group_class_order_email_required", default: false, null: false
    t.boolean "group_class_order_disease_field", default: false, null: false
    t.boolean "group_class_order_disease_required", default: false, null: false
    t.string "question_description"
    t.date "started_at"
    t.date "finished_at"
    t.string "payment_succeeded_url"
    t.boolean "registration_form_suitable_started_date_field", default: true
    t.boolean "credit_installment"
    t.boolean "credit_subscription"
    t.string "period_type"
    t.integer "frequency"
    t.integer "exec_times"
    t.boolean "enable_coupon_code", default: true
    t.integer "org_id"
    t.string "suitable_started_date_prompt_text"
    t.jsonb "data"
    t.boolean "payment_method_on_site"
    t.integer "terms_of_service_id"
    t.integer "personal_pricing_id"
    t.string "plan_section_title"
  end

  create_table "relationships", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "client_id"
    t.datetime "connect_date"
    t.datetime "end_date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.string "created_by"
    t.integer "org_id"
    t.index ["client_id", "user_id"], name: "index_relationships_on_client_id_and_user_id"
    t.index ["client_id"], name: "index_relationships_on_client_id"
    t.index ["user_id"], name: "index_relationships_on_user_id"
  end

  create_table "report_orders", force: :cascade do |t|
    t.integer "gco_id"
    t.integer "gco_client_id"
    t.string "gco_aasm_state"
    t.string "gco_name"
    t.string "gco_plan"
    t.string "gco_cellphone_number"
    t.string "gco_email"
    t.string "gco_county"
    t.string "gco_district"
    t.string "gco_address"
    t.string "gco_zip_code"
    t.date "gco_preferred_started_date"
    t.datetime "gco_created_at"
    t.string "gco_disease"
    t.string "gco_payment_method"
    t.string "gco_registration_code"
    t.string "ecpo_merchant_trade_no"
    t.integer "ecpo_total_amount"
    t.string "ecpo_trade_desc"
    t.string "ecpo_item_name"
    t.string "ecpo_aasm_state"
    t.datetime "ecpo_created_at"
    t.string "coupon_code_name"
    t.string "coupon_code_code"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "reservations", id: :serial, force: :cascade do |t|
    t.date "date"
    t.integer "hour"
    t.integer "minute"
    t.integer "duration"
    t.string "service_type"
    t.integer "org_id"
    t.integer "registration_form_plan_id"
    t.integer "group_class_order_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["group_class_order_id"], name: "index_reservations_on_group_class_order_id"
    t.index ["org_id"], name: "index_reservations_on_org_id"
    t.index ["registration_form_plan_id"], name: "index_reservations_on_registration_form_plan_id"
  end

  create_table "review_groups", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "org_id"
    t.integer "group_class_id"
    t.string "name"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["group_class_id"], name: "index_review_groups_on_group_class_id"
    t.index ["org_id"], name: "index_review_groups_on_org_id"
    t.index ["user_id"], name: "index_review_groups_on_user_id"
  end

  create_table "roles", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "resource_id"
    t.string "resource_type"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "org_id"
    t.index ["name", "resource_type", "resource_id"], name: "index_roles_on_name_and_resource_type_and_resource_id"
    t.index ["name"], name: "index_roles_on_name"
  end

  create_table "rpush_apps", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.string "environment"
    t.text "certificate"
    t.string "password"
    t.integer "connections", default: 1, null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string "type", null: false
    t.string "auth_key"
    t.string "client_id"
    t.string "client_secret"
    t.string "access_token"
    t.datetime "access_token_expiration"
    t.text "apn_key"
    t.string "apn_key_id"
    t.string "team_id"
    t.string "bundle_id"
    t.boolean "feedback_enabled", default: true
    t.index ["id", "type"], name: "index_rpush_apps_on_id_and_type"
  end

  create_table "rpush_feedback", id: :serial, force: :cascade do |t|
    t.string "device_token"
    t.datetime "failed_at", null: false
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer "app_id"
    t.index ["app_id"], name: "index_rpush_feedback_on_app_id"
    t.index ["device_token"], name: "index_rpush_feedback_on_device_token"
  end

  create_table "rpush_notifications", id: :serial, force: :cascade do |t|
    t.integer "badge"
    t.string "device_token"
    t.string "sound"
    t.text "alert"
    t.text "data"
    t.integer "expiry", default: 86400
    t.boolean "delivered", default: false, null: false
    t.datetime "delivered_at"
    t.boolean "failed", default: false, null: false
    t.datetime "failed_at"
    t.integer "error_code"
    t.text "error_description"
    t.datetime "deliver_after"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.boolean "alert_is_json", default: false, null: false
    t.string "type", null: false
    t.string "collapse_key"
    t.boolean "delay_while_idle", default: false, null: false
    t.text "registration_ids"
    t.integer "app_id", null: false
    t.integer "retries", default: 0
    t.string "uri"
    t.datetime "fail_after"
    t.boolean "processing", default: false, null: false
    t.integer "priority"
    t.text "url_args"
    t.string "category"
    t.boolean "content_available", default: false, null: false
    t.text "notification"
    t.boolean "mutable_content", default: false, null: false
    t.string "external_device_id"
    t.string "thread_id"
    t.boolean "dry_run", default: false, null: false
    t.boolean "sound_is_json", default: false
    t.index ["app_id"], name: "index_rpush_notifications_on_app_id"
    t.index ["delivered", "failed", "processing", "deliver_after", "created_at"], name: "index_rpush_notifications_multi", where: "((NOT delivered) AND (NOT failed))"
    t.index ["id", "type"], name: "index_rpush_notifications_on_id_and_type"
  end

  create_table "schedule_service_logs", id: :serial, force: :cascade do |t|
    t.string "service_class"
    t.string "service_method"
    t.integer "target_id"
    t.string "target_type"
    t.string "aasm_state"
    t.string "remarks"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.index ["created_at"], name: "index_schedule_service_logs_on_created_at"
    t.index ["service_class"], name: "index_schedule_service_logs_on_service_class"
    t.index ["service_method"], name: "index_schedule_service_logs_on_service_method"
    t.index ["updated_at"], name: "index_schedule_service_logs_on_updated_at"
  end

  create_table "scheduled_messages", id: :serial, force: :cascade do |t|
    t.string "content"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "sender_role_id"
    t.integer "receiver_chat_kind"
    t.integer "role_id"
    t.string "media_type"
    t.string "media_url"
  end

  create_table "scheduled_notifications", id: :serial, force: :cascade do |t|
    t.string "title"
    t.string "text"
    t.string "sender_type"
    t.integer "sender_id"
    t.string "recipient_type"
    t.integer "recipient_id"
    t.string "provider"
    t.boolean "push"
    t.datetime "scheduled_at"
    t.datetime "dispatch_at"
    t.datetime "sent_at"
    t.integer "notification_id"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "external_id"
    t.string "uid"
    t.index ["external_id"], name: "index_scheduled_notifications_on_external_id"
    t.index ["notification_id"], name: "index_scheduled_notifications_on_notification_id"
    t.index ["uid"], name: "index_scheduled_notifications_on_uid"
  end

  create_table "service_chat_attachments", id: :serial, force: :cascade do |t|
    t.integer "sender_id"
    t.string "sender_type"
    t.string "chatroom_id"
    t.string "bucket"
    t.string "key"
    t.string "url"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "service_chat_rooms", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "service_name"
    t.string "service_avatar"
    t.string "kind"
    t.integer "app_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["app_id"], name: "index_service_chat_rooms_on_app_id"
  end

  create_table "sessions", id: :serial, force: :cascade do |t|
    t.string "session_id", null: false
    t.text "data"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.index ["session_id"], name: "index_sessions_on_session_id", unique: true
    t.index ["updated_at"], name: "index_sessions_on_updated_at"
  end

  create_table "shares", id: :serial, force: :cascade do |t|
    t.string "record_type"
    t.integer "record_id"
    t.integer "new_post_id"
    t.integer "sender_id"
    t.string "sender_type"
    t.integer "target_id"
    t.string "target_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "shows", id: :serial, force: :cascade do |t|
    t.string "showable_type"
    t.integer "showable_id"
    t.string "sender_type"
    t.integer "sender_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["sender_type", "sender_id", "showable_type", "showable_id"], name: "read"
  end

  create_table "social_blocks", id: :serial, force: :cascade do |t|
    t.integer "blocker_id"
    t.string "blocker_type"
    t.integer "blocked_id"
    t.string "blocked_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["blocked_type", "blocked_id"], name: "index_social_blocks_on_blocked_type_and_blocked_id"
    t.index ["blocker_type", "blocker_id"], name: "index_social_blocks_on_blocker_type_and_blocker_id"
  end

  create_table "social_events", id: :serial, force: :cascade do |t|
    t.integer "actor_id"
    t.string "actor_type"
    t.integer "action_id"
    t.string "action_type"
    t.integer "object_id"
    t.string "object_type"
    t.integer "target_id"
    t.string "target_type"
    t.string "verb"
    t.datetime "created_at", null: false
    t.index ["action_type", "action_id"], name: "index_social_events_on_action_type_and_action_id"
    t.index ["actor_type", "actor_id"], name: "index_social_events_on_actor_type_and_actor_id"
    t.index ["object_type", "object_id"], name: "index_social_events_on_object_type_and_object_id"
    t.index ["target_type", "target_id"], name: "index_social_events_on_target_type_and_target_id"
    t.index ["verb"], name: "index_social_events_on_verb"
  end

  create_table "social_group_memberships", id: :serial, force: :cascade do |t|
    t.string "member_type"
    t.integer "member_id"
    t.integer "social_group_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["social_group_id"], name: "index_social_group_memberships_on_social_group_id"
  end

  create_table "social_groups", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "description"
    t.integer "org_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["org_id"], name: "index_social_groups_on_org_id"
  end

  create_table "social_identities", id: :serial, force: :cascade do |t|
    t.integer "member_id"
    t.string "member_type"
    t.integer "realm_id"
    t.string "nick_name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["member_type", "nick_name"], name: "index_social_identities_on_member_type_and_nick_name"
  end

  create_table "social_reports", id: :serial, force: :cascade do |t|
    t.integer "record_id"
    t.string "record_type"
    t.integer "member_id"
    t.string "member_type"
    t.string "reason"
    t.boolean "resolved", default: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "realm_id"
  end

  create_table "subscription_service_leaves", force: :cascade do |t|
    t.date "start_date"
    t.date "finish_date"
    t.bigint "client_id"
    t.bigint "group_class_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_subscription_service_leaves_on_client_id"
    t.index ["group_class_id"], name: "index_subscription_service_leaves_on_group_class_id"
  end

  create_table "subscription_services", id: :serial, force: :cascade do |t|
    t.string "subscriber_type"
    t.integer "subscriber_id"
    t.date "started_at"
    t.date "finished_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "product_type"
    t.integer "product_id"
    t.string "aasm_state"
    t.string "order_type"
    t.integer "order_id"
    t.integer "relationship_id"
    t.integer "care_receiver_id"
  end

  create_table "suggests", id: :serial, force: :cascade do |t|
    t.string "first_name"
    t.string "last_name"
    t.string "country"
    t.string "country_code"
    t.string "phone_number"
    t.text "content"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "email", null: false
    t.integer "user_id"
    t.string "title"
  end

  create_table "survey_answers", id: :serial, force: :cascade do |t|
    t.integer "question_id"
    t.integer "survey_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "answer"
    t.index ["survey_id"], name: "index_survey_answers_on_survey_id"
  end

  create_table "survey_forms", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "questions_count"
    t.string "provider_type"
    t.integer "provider_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "uniqueness", default: true
    t.string "notice_info", default: ""
    t.boolean "retakeable"
    t.string "direct_title"
    t.string "footer"
    t.integer "target_id"
    t.string "target_type"
    t.jsonb "data"
  end

  create_table "surveys", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "time"
    t.string "target_type"
    t.integer "target_id"
    t.jsonb "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "footer"
    t.integer "questions_count"
    t.integer "survey_form_id"
    t.string "provider"
    t.datetime "notification_sent_at"
    t.datetime "dispatch_at"
    t.date "end_date"
    t.boolean "retakeable"
    t.datetime "taken_at"
    t.index ["provider", "created_at"], name: "index_surveys_on_provider_and_created_at"
    t.index ["survey_form_id"], name: "index_surveys_on_survey_form_id"
    t.index ["target_id", "target_type", "survey_form_id"], name: "index_surveys_on_target_and_survey_form"
    t.index ["time", "target_type", "target_id", "dispatch_at"], name: "index_surveys_on_undispatched_notifications"
  end

  create_table "tagging_articles", id: :serial, force: :cascade do |t|
    t.integer "tag_id"
    t.integer "article_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["article_id"], name: "index_tagging_articles_on_article_id"
    t.index ["tag_id"], name: "index_tagging_articles_on_tag_id"
  end

  create_table "tagging_essays", id: :serial, force: :cascade do |t|
    t.integer "tag_id"
    t.integer "essay_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "taggings", id: :serial, force: :cascade do |t|
    t.string "tagable_type"
    t.integer "tagable_id"
    t.integer "tag_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["tag_id"], name: "index_taggings_on_tag_id"
    t.index ["tagable_type", "tagable_id"], name: "index_taggings_on_tagable_type_and_tagable_id"
  end

  create_table "tags", id: :serial, force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "org_id"
    t.integer "channel_id"
    t.jsonb "data"
    t.integer "realm_id"
  end

  create_table "temp_client_photo_items", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.integer "photo_id"
    t.integer "item_id"
    t.string "item_type"
    t.jsonb "data"
    t.date "date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["client_id"], name: "index_temp_client_photo_items_on_client_id"
    t.index ["photo_id"], name: "index_temp_client_photo_items_on_photo_id"
  end

  create_table "temp_client_photos", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.string "bucket"
    t.string "key"
    t.string "url"
    t.text "text"
    t.jsonb "data"
    t.date "date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "public", default: false
    t.integer "comments_count", default: 0
    t.integer "temp_client_id"
    t.index ["client_id"], name: "index_temp_client_photos_on_client_id"
    t.index ["temp_client_id"], name: "index_temp_client_photos_on_temp_client_id"
  end

  create_table "temp_clients", id: :serial, force: :cascade do |t|
    t.integer "org_id"
    t.string "uid"
    t.string "name"
    t.string "provider"
    t.string "mobile"
    t.string "email"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.date "birthday"
    t.index ["org_id"], name: "index_temp_clients_on_org_id"
    t.index ["uid"], name: "index_temp_clients_on_uid"
  end

  create_table "terms_of_services", id: :serial, force: :cascade do |t|
    t.string "name"
    t.text "content"
    t.integer "org_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["org_id"], name: "index_terms_of_services_on_org_id"
  end

  create_table "third_party_access_tokens", id: :serial, force: :cascade do |t|
    t.string "token"
    t.datetime "expired_at"
    t.string "provider"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.jsonb "data"
    t.integer "client_id"
  end

  create_table "third_party_activities", id: :serial, force: :cascade do |t|
    t.string "name", null: false
    t.string "kind"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "third_party_auths", id: :serial, force: :cascade do |t|
    t.integer "client_id"
    t.string "name", null: false
    t.string "uid", null: false
    t.string "token"
    t.string "refresh_token"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.datetime "synced_at"
    t.datetime "expires_at"
    t.index ["client_id"], name: "index_third_party_auths_on_client_id"
  end

  create_table "topics", id: :serial, force: :cascade do |t|
    t.string "name"
    t.integer "category_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["category_id"], name: "index_topics_on_category_id"
  end

  create_table "upload_files", id: :serial, force: :cascade do |t|
    t.string "url"
    t.integer "org_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "filename"
    t.string "member_type"
    t.integer "member_id"
    t.string "key"
    t.string "data_type"
    t.integer "media_type"
    t.string "category"
    t.jsonb "data"
    t.integer "source_id"
    t.string "source_type"
    t.index ["source_id", "source_type"], name: "index_upload_files_on_source_id_and_source_type"
  end

  create_table "user_client_settings", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "client_id"
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "pinned"
    t.index ["client_id"], name: "index_user_client_settings_on_client_id"
    t.index ["pinned"], name: "index_user_client_settings_on_pinned"
    t.index ["user_id"], name: "index_user_client_settings_on_user_id"
  end

  create_table "user_invitations", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "invitation_user_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "status", default: false
    t.index ["user_id"], name: "index_user_invitations_on_user_id"
  end

  create_table "user_role_relations", id: :serial, force: :cascade do |t|
    t.integer "user_id"
    t.integer "role_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["role_id"], name: "index_user_role_relations_on_role_id"
    t.index ["user_id"], name: "index_user_role_relations_on_user_id"
  end

  create_table "users", id: :serial, force: :cascade do |t|
    t.string "first_name", limit: 255, null: false
    t.string "email", limit: 255, null: false
    t.string "salt"
    t.string "encrypted_password"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.string "mobile"
    t.datetime "update_at_password"
    t.datetime "user_identity"
    t.string "user_identity_content"
    t.string "registration_number"
    t.string "practicing_registration_sites"
    t.string "resume"
    t.text "food_star"
    t.text "food_db", default: "[{\"name\":\"cofit\",\"isChecked\":true},{\"name\":\"政府2018\",\"isChecked\":true},{\"name\":\"自訂\",\"isChecked\":true},{\"name\":\"組織\",\"isChecked\":false}]", null: false
    t.integer "sign_in_count", default: 0, null: false
    t.datetime "current_sign_in_at"
    t.string "current_sign_in_ip"
    t.datetime "last_sign_in_at"
    t.string "last_sign_in_ip"
    t.string "confirmation_token"
    t.datetime "confirmed_at"
    t.datetime "confirmation_sent_at"
    t.integer "client_limit", default: 5
    t.json "identiy"
    t.string "vip_level", default: "Free"
    t.json "career"
    t.string "exclusive_id"
    t.string "invitation_code"
    t.string "license_status", default: "尚未上傳證書"
    t.string "license_file_name"
    t.string "license_content_type"
    t.integer "license_file_size"
    t.datetime "license_updated_at"
    t.string "avatar_file_name"
    t.string "avatar_content_type"
    t.integer "avatar_file_size"
    t.datetime "avatar_updated_at"
    t.string "last_name"
    t.string "reserve_email"
    t.integer "plan_id"
    t.string "provider", default: "email", null: false
    t.string "uid", default: "", null: false
    t.string "reset_password_token"
    t.datetime "reset_password_sent_at"
    t.datetime "remember_created_at"
    t.string "image"
    t.text "tokens", default: "{}"
    t.string "unconfirmed_email"
    t.jsonb "extra"
    t.jsonb "org_cert"
    t.integer "connected_client_limit", default: 1
    t.integer "archived_client_limit", default: 0
    t.integer "followings_count", default: 0, null: false
    t.integer "followers_count", default: 0, null: false
    t.string "locale", default: "zh-TW"
    t.integer "level"
    t.text "issues"
    t.text "remarks"
    t.string "available_time"
    t.string "avatar_url"
    t.string "identity_number"
    t.date "birthday"
    t.integer "posts_count", default: 0
    t.string "nick_name"
    t.string "registration_code"
    t.string "gender"
    t.boolean "allow_password_change", default: false
    t.jsonb "current"
    t.string "real_name"
    t.index ["email"], name: "ix_email", unique: true
    t.index ["plan_id"], name: "index_users_on_plan_id"
  end

  create_table "users_roles", id: false, force: :cascade do |t|
    t.integer "user_id"
    t.integer "role_id"
    t.index ["user_id", "role_id"], name: "index_users_roles_on_user_id_and_role_id"
  end

  create_table "vendor_requests", id: :serial, force: :cascade do |t|
    t.string "source"
    t.string "action"
    t.jsonb "data"
    t.string "brand_name"
    t.jsonb "client_info"
    t.string "data_source"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.boolean "finished", default: false
    t.index ["brand_name"], name: "index_vendor_requests_on_brand_name"
  end

  create_table "wechat_access_tokens", id: :serial, force: :cascade do |t|
    t.string "access_token"
    t.datetime "expired_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "app_id"
  end

  create_table "wechat_form_ids", id: :serial, force: :cascade do |t|
    t.string "form_id"
    t.integer "client_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "aasm_state"
    t.integer "app_id"
    t.string "kind"
    t.index ["client_id"], name: "index_wechat_form_ids_on_client_id"
  end

  create_table "wechat_forms", id: :serial, force: :cascade do |t|
    t.string "touser"
    t.string "template_id"
    t.string "page"
    t.string "form_id"
    t.string "emphasis_keyword"
    t.string "data"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.string "response_body"
  end

  create_table "wechat_templates", id: :serial, force: :cascade do |t|
    t.string "name"
    t.string "title"
    t.string "template_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.integer "app_id"
  end

  create_table "wechat_webhooks", id: :serial, force: :cascade do |t|
    t.string "unabridged_params"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "welcome_messages", id: :serial, force: :cascade do |t|
    t.integer "program_id"
    t.integer "scheduled_message_id"
    t.integer "order"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["program_id"], name: "index_welcome_messages_on_program_id"
    t.index ["scheduled_message_id"], name: "index_welcome_messages_on_scheduled_message_id"
  end

  add_foreign_key "access_tokens", "users", name: "access_tokens_user_id_fkey"
  add_foreign_key "batch_action_histories", "users"
  add_foreign_key "branch_memberships", "branches"
  add_foreign_key "branch_treatments", "branches"
  add_foreign_key "branches", "orgs"
  add_foreign_key "campaign_actions", "campaign_stages"
  add_foreign_key "campaign_actions", "campaigns"
  add_foreign_key "campaign_actions", "clients"
  add_foreign_key "campaign_actions", "orgs"
  add_foreign_key "campaign_stage_settings", "campaign_stages"
  add_foreign_key "campaign_stage_settings", "campaigns"
  add_foreign_key "campaigns", "orgs"
  add_foreign_key "chat_announcements", "chats"
  add_foreign_key "chat_attachments", "chats"
  add_foreign_key "choices", "questions"
  add_foreign_key "clients", "users", name: "consulting_clients_user_id_fkey"
  add_foreign_key "clinic_soaps", "clients"
  add_foreign_key "consulting_food_records", "clients"
  add_foreign_key "course_set_course_relationships", "course_sets"
  add_foreign_key "course_set_template_course_relationships", "course_set_templates"
  add_foreign_key "food_calculator_product_ingredients", "food_calculator_products", name: "food_calculator_product_ingredi_food_calculator_product_id_fkey"
  add_foreign_key "food_calculator_product_ingredients", "ingredients", name: "food_calculator_product_ingredients_ingredient_id_fkey"
  add_foreign_key "food_calculator_products", "food_calculator_projects", name: "food_calculator_products_food_calculator_project_id_fkey"
  add_foreign_key "food_calculator_project_notes", "food_calculator_projects", name: "food_calculator_project_notes_food_calculator_project_id_fkey"
  add_foreign_key "food_calculator_projects", "users", name: "projects_user_id_fkey"
  add_foreign_key "food_ingredients", "ingredients", name: "food_ingredients_ingredient_id_fkey"
  add_foreign_key "food_tags", "users", name: "food_tags_user_id_fkey"
  add_foreign_key "group_class_activities", "group_classes"
  add_foreign_key "group_class_campaigns", "orgs"
  add_foreign_key "group_class_redemption_codes", "group_class_campaigns"
  add_foreign_key "group_class_user_relations", "group_classes"
  add_foreign_key "group_class_user_relations", "users"
  add_foreign_key "health_bank_authorizations", "clients"
  add_foreign_key "health_bank_authorizations", "users"
  add_foreign_key "health_banks", "clients"
  add_foreign_key "jobs", "campaigns"
  add_foreign_key "message_template_items", "message_templates"
  add_foreign_key "note_assets", "notes"
  add_foreign_key "note_items", "notes"
  add_foreign_key "org_referral_set_relations", "orgs"
  add_foreign_key "org_referral_set_relations", "referral_sets"
  add_foreign_key "personal_ingredients", "users", name: "personal_ingredients_user_id_fkey"
  add_foreign_key "personal_tags", "users", name: "personal_tags_user_id_fkey"
  add_foreign_key "pharmacy_stock_outgoings", "pharmacy_stock_incomings", column: "incoming_id"
  add_foreign_key "program_course_set_template_relationships", "course_set_templates"
  add_foreign_key "program_course_set_template_relationships", "programs"
  add_foreign_key "program_message_template_relationships", "message_templates"
  add_foreign_key "program_message_template_relationships", "programs"
  add_foreign_key "program_topics", "programs"
  add_foreign_key "question_sets", "question_sets", column: "parent_id"
  add_foreign_key "question_sets", "survey_forms"
  add_foreign_key "questions", "question_sets"
  add_foreign_key "recipe_ingredients", "ingredients", name: "recipe_ingredients_ingredient_id_fkey"
  add_foreign_key "recipe_ingredients", "recipes", name: "recipe_ingredients_recipe_id_fkey"
  add_foreign_key "recipes", "users", name: "recipes_user_id_fkey"
  add_foreign_key "referral_code_referral_set_relations", "referral_codes"
  add_foreign_key "referral_code_referral_set_relations", "referral_sets"
  add_foreign_key "referral_set_items", "referral_sets"
  add_foreign_key "referral_set_items", "registration_form_plans"
  add_foreign_key "registration_form_assistant_relationships", "registration_form_plans"
  add_foreign_key "registration_form_assistant_relationships", "users"
  add_foreign_key "registration_form_coupon_code_ecpay_order_relationships", "ecpay_orders"
  add_foreign_key "registration_form_coupon_code_ecpay_order_relationships", "registration_form_coupon_codes"
  add_foreign_key "registration_form_coupon_codes", "registration_forms"
  add_foreign_key "registration_form_master_dietician_relationships", "registration_form_plans"
  add_foreign_key "registration_form_master_dietician_relationships", "users"
  add_foreign_key "registration_form_plan_user_relations", "registration_form_plans"
  add_foreign_key "registration_form_plan_user_relations", "roles"
  add_foreign_key "registration_form_plan_user_relations", "users"
  add_foreign_key "registration_form_plans", "registration_forms"
  add_foreign_key "registration_form_questions", "registration_forms"
  add_foreign_key "registration_form_reminders", "registration_forms"
  add_foreign_key "registration_form_suitable_started_dates", "registration_forms"
  add_foreign_key "registration_form_titles", "registration_forms"
  add_foreign_key "reservations", "group_class_orders"
  add_foreign_key "reservations", "orgs"
  add_foreign_key "reservations", "registration_form_plans"
  add_foreign_key "review_groups", "group_classes"
  add_foreign_key "review_groups", "orgs"
  add_foreign_key "review_groups", "users"
  add_foreign_key "scheduled_notifications", "notifications"
  add_foreign_key "service_chat_rooms", "apps"
  add_foreign_key "social_group_memberships", "social_groups"
  add_foreign_key "social_groups", "orgs"
  add_foreign_key "subscription_service_leaves", "clients"
  add_foreign_key "subscription_service_leaves", "group_classes"
  add_foreign_key "survey_answers", "surveys"
  add_foreign_key "taggings", "tags"
  add_foreign_key "terms_of_services", "orgs"
  add_foreign_key "topics", "categories"
  add_foreign_key "user_role_relations", "roles"
  add_foreign_key "user_role_relations", "users"
  add_foreign_key "wechat_form_ids", "clients"
  add_foreign_key "welcome_messages", "programs"
  add_foreign_key "welcome_messages", "scheduled_messages"

  create_view "medicines_lower_than_safe_stock_amount_with_timestamps", sql_definition: <<-SQL
      WITH medicines_with_stocks AS (
           SELECT medicines.id,
              medicines.code,
              medicines.english_name,
              medicines.safe_stock_amount,
              stocks.outputable_amount,
              stocks.balance_amount
             FROM (pharmacy_medicines medicines
               JOIN pharmacy_stocks stocks ON ((medicines.id = stocks.medicine_id)))
            WHERE (stocks.outputable_amount < (medicines.safe_stock_amount)::numeric)
          ), latest_pharmacy_stock_outgoings AS (
           SELECT pharmacy_stock_outgoings.medicine_id,
              max(pharmacy_stock_outgoings.created_at) AS created_at
             FROM pharmacy_stock_outgoings
            GROUP BY pharmacy_stock_outgoings.medicine_id
          )
   SELECT medicines_with_stocks.id,
      medicines_with_stocks.code,
      medicines_with_stocks.english_name,
      medicines_with_stocks.safe_stock_amount,
      medicines_with_stocks.outputable_amount,
      medicines_with_stocks.balance_amount,
      latest_pharmacy_stock_outgoings.medicine_id,
      latest_pharmacy_stock_outgoings.created_at
     FROM (medicines_with_stocks
       LEFT JOIN latest_pharmacy_stock_outgoings ON ((medicines_with_stocks.id = latest_pharmacy_stock_outgoings.medicine_id)))
    ORDER BY latest_pharmacy_stock_outgoings.created_at DESC;
  SQL
  create_view "outputable_pharmacy_stock_incomings", sql_definition: <<-SQL
      WITH outgoings_grouped_by_incoming_id AS (
           SELECT pharmacy_stock_outgoings.incoming_id,
              sum(pharmacy_stock_outgoings.amount) AS outgoings_amount_sum
             FROM pharmacy_stock_outgoings
            GROUP BY pharmacy_stock_outgoings.incoming_id
          )
   SELECT incomings.id,
      incomings.medicine_id,
      incomings.order_id,
      incomings.source_type,
      incomings.source_id,
      incomings.amount,
      incomings.expiration_date,
      incomings.date,
      incomings.created_at,
      incomings.updated_at,
      outgoings.incoming_id,
      outgoings.outgoings_amount_sum
     FROM (pharmacy_stock_incomings incomings
       LEFT JOIN outgoings_grouped_by_incoming_id outgoings ON ((incomings.id = outgoings.incoming_id)))
    WHERE
          CASE
              WHEN (outgoings.outgoings_amount_sum IS NULL) THEN true
              ELSE ((incomings.amount - outgoings.outgoings_amount_sum) > (0)::numeric)
          END;
  SQL
  create_view "medicines_days_to_expiration_lower_than_expiration_alert_days", sql_definition: <<-SQL
      WITH outputable_incomings AS (
           SELECT incomings_1.id,
              incomings_1.medicine_id,
              incomings_1.order_id,
              incomings_1.source_type,
              incomings_1.source_id,
              incomings_1.amount,
              incomings_1.expiration_date,
              incomings_1.date,
              incomings_1.created_at,
              incomings_1.updated_at,
              incomings_1.incoming_id,
              incomings_1.outgoings_amount_sum,
              input_orders.id,
              input_orders.user_id,
              input_orders.org_id,
              input_orders.branch_id,
              input_orders.source_order_type,
              input_orders.source_order_id,
              input_orders.serial_number,
              input_orders.date,
              input_orders.datetime,
              input_orders.created_at,
              input_orders.updated_at
             FROM (outputable_pharmacy_stock_incomings incomings_1
               JOIN pharmacy_stock_input_orders input_orders ON ((incomings_1.order_id = input_orders.id)))
          )
   SELECT medicines.id,
      medicines.branch_id,
      medicines.status,
      medicines.code,
      medicines.code_color,
      medicines.english_name,
      medicines.chinese_name,
      medicines.generic_name,
      medicines.usage,
      medicines.route,
      medicines.minimum_unit,
      medicines.package_unit,
      medicines.package_amount,
      medicines.safe_stock_amount,
      medicines.side_effect,
      medicines.notice,
      medicines.self_pay_fee,
      medicines.expiration_alert_days,
      medicines.created_at,
      medicines.updated_at,
      incomings.serial_number,
      incomings.expiration_date,
      (incomings.expiration_date - date(timezone('Asia/Taipei'::text, now()))) AS days_to_expire
     FROM (pharmacy_medicines medicines
       JOIN outputable_incomings incomings(id, medicine_id, order_id, source_type, source_id, amount, expiration_date, date, created_at, updated_at, incoming_id, outgoings_amount_sum, id_1, user_id, org_id, branch_id, source_order_type, source_order_id, serial_number, date_1, datetime, created_at_1, updated_at_1) ON ((medicines.id = incomings.medicine_id)))
    WHERE (((incomings.expiration_date - date(timezone('Asia/Taipei'::text, now()))) > 0) AND ((incomings.expiration_date - date(timezone('Asia/Taipei'::text, now()))) <= medicines.expiration_alert_days));
  SQL
end
