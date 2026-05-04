/**
 * 主键重构与索引优化
 *
 * 变更说明：
 *     1. 删除自增 id 列，将 UUID 业务 id 设为主键
 *     2. 添加常用查询字段索引优化查询性能
 */

-- =====================================================
-- 第一部分：删除自增 ID，将 UUID 业务 ID 设为主键
-- =====================================================

-- ----------------------------
-- 1. t_authorization 表
-- ----------------------------
ALTER TABLE "t_authorization" RENAME COLUMN "id" TO "authorization_id";
ALTER TABLE "t_authorization" ALTER COLUMN "authorization_id" TYPE varchar(50);
ALTER TABLE "t_authorization" ALTER COLUMN "authorization_id" DROP DEFAULT;
ALTER TABLE "t_authorization" DROP CONSTRAINT IF EXISTS "t_authorization_pkey";
ALTER TABLE "t_authorization" ADD CONSTRAINT "t_authorization_pkey" PRIMARY KEY ("authorization_id");
DROP SEQUENCE IF EXISTS "t_authorization_id_seq" CASCADE;

-- ----------------------------
-- 2. t_authorize 表
-- ----------------------------
ALTER TABLE "t_authorize" DROP CONSTRAINT IF EXISTS "t_authority_pkey";
ALTER TABLE "t_authorize" ADD CONSTRAINT "t_authorize_pkey" PRIMARY KEY ("authorize_id");
ALTER TABLE "t_authorize" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_authority_id_seq" CASCADE;

-- ----------------------------
-- 3. t_client 表
-- ----------------------------
ALTER TABLE "t_client" DROP CONSTRAINT IF EXISTS "t_client_pkey";
ALTER TABLE "t_client" ADD CONSTRAINT "t_client_pkey" PRIMARY KEY ("client_id");
ALTER TABLE "t_client" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_client_id_seq" CASCADE;

-- ----------------------------
-- 4. t_oidc_claim 表
-- ----------------------------
ALTER TABLE "t_oidc_claim" DROP CONSTRAINT IF EXISTS "t_oidc_claim_pkey";
ALTER TABLE "t_oidc_claim" ADD CONSTRAINT "t_oidc_claim_pkey" PRIMARY KEY ("claim_id");
ALTER TABLE "t_oidc_claim" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_oidc_claim_id_seq" CASCADE;

-- ----------------------------
-- 5. t_oidc_scope 表
-- ----------------------------
ALTER TABLE "t_oidc_scope" DROP CONSTRAINT IF EXISTS "t_scope_pkey";
ALTER TABLE "t_oidc_scope" ADD CONSTRAINT "t_oidc_scope_pkey" PRIMARY KEY ("scope_id");
ALTER TABLE "t_oidc_scope" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_scope_id_seq" CASCADE;

-- ----------------------------
-- 6. t_permission 表
-- ----------------------------
ALTER TABLE "t_permission" DROP CONSTRAINT IF EXISTS "t_permission_pkey";
ALTER TABLE "t_permission" ADD CONSTRAINT "t_permission_pkey" PRIMARY KEY ("permission_id");
ALTER TABLE "t_permission" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_permission_id_seq" CASCADE;

-- ----------------------------
-- 7. t_permission_exp 表
-- ----------------------------
ALTER TABLE "t_permission_exp" DROP CONSTRAINT IF EXISTS "t_expression_pkey";
ALTER TABLE "t_permission_exp" ADD CONSTRAINT "t_permission_exp_pkey" PRIMARY KEY ("expression_id");
ALTER TABLE "t_permission_exp" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_expression_id_seq" CASCADE;

-- ----------------------------
-- 8. t_resource 表
-- ----------------------------
ALTER TABLE "t_resource" DROP CONSTRAINT IF EXISTS "t_resource_pkey";
ALTER TABLE "t_resource" ADD CONSTRAINT "t_resource_pkey" PRIMARY KEY ("resource_id");
ALTER TABLE "t_resource" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_resource_id_seq" CASCADE;

-- ----------------------------
-- 9. t_resource_group 表
-- ----------------------------
ALTER TABLE "t_resource_group" DROP CONSTRAINT IF EXISTS "t_resource_group_pkey";
ALTER TABLE "t_resource_group" ADD CONSTRAINT "t_resource_group_pkey" PRIMARY KEY ("resource_group_id");
ALTER TABLE "t_resource_group" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_resource_group_id_seq" CASCADE;

-- ----------------------------
-- 10. t_role 表
-- ----------------------------
ALTER TABLE "t_role" DROP CONSTRAINT IF EXISTS "t_role_pkey";
ALTER TABLE "t_role" ADD CONSTRAINT "t_role_pkey" PRIMARY KEY ("role_id");
ALTER TABLE "t_role" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_role_id_seq" CASCADE;

-- ----------------------------
-- 11. t_user 表
-- ----------------------------
ALTER TABLE "t_user" DROP CONSTRAINT IF EXISTS "t_user_pk";
ALTER TABLE "t_user" ADD CONSTRAINT "t_user_pkey" PRIMARY KEY ("user_id");
ALTER TABLE "t_user" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_user_id_seq" CASCADE;

-- ----------------------------
-- 12. t_user_attr 表
-- ----------------------------
ALTER TABLE "t_user_attr" DROP CONSTRAINT IF EXISTS "t_user_attr_pkey";
ALTER TABLE "t_user_attr" ADD CONSTRAINT "t_user_attr_pkey" PRIMARY KEY ("attr_id");
ALTER TABLE "t_user_attr" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_user_attr_id_seq" CASCADE;

-- ----------------------------
-- 13. t_user_group 表
-- ----------------------------
ALTER TABLE "t_user_group" DROP CONSTRAINT IF EXISTS "t_user_group_pkey";
ALTER TABLE "t_user_group" ADD CONSTRAINT "t_user_group_pkey" PRIMARY KEY ("user_group_id");
ALTER TABLE "t_user_group" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_user_group_id_seq" CASCADE;

-- ----------------------------
-- 14. t_identity_source_provider 表
-- ----------------------------
ALTER TABLE "t_identity_source_provider" DROP CONSTRAINT IF EXISTS "t_identity_source_provider_pkey";
ALTER TABLE "t_identity_source_provider" ADD CONSTRAINT "t_identity_source_provider_pkey" PRIMARY KEY ("provider_id");
ALTER TABLE "t_identity_source_provider" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_identity_source_provider_id_seq" CASCADE;

-- ----------------------------
-- 15. t_identity_source_registration 表
-- ----------------------------
ALTER TABLE "t_identity_source_registration" DROP CONSTRAINT IF EXISTS "t_identity_source_registration_pkey";
ALTER TABLE "t_identity_source_registration" ADD CONSTRAINT "t_identity_source_registration_pkey" PRIMARY KEY ("registration_id");
ALTER TABLE "t_identity_source_registration" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_identity_source_registration_id_seq" CASCADE;

-- ----------------------------
-- 16. t_third_account 表
-- ----------------------------
ALTER TABLE "t_third_account" RENAME COLUMN "id" TO "third_account_id";
ALTER TABLE "t_third_account" ALTER COLUMN "third_account_id" TYPE varchar(50);
ALTER TABLE "t_third_account" ALTER COLUMN "third_account_id" DROP DEFAULT;
ALTER TABLE "t_third_account" DROP CONSTRAINT IF EXISTS "t_third_account_pkey";
ALTER TABLE "t_third_account" ADD CONSTRAINT "t_third_account_pkey" PRIMARY KEY ("third_account_id");
DROP SEQUENCE IF EXISTS "t_third_account_id_seq" CASCADE;

-- ----------------------------
-- 17. t_password_policy 表
-- ----------------------------
ALTER TABLE "t_password_policy" DROP CONSTRAINT IF EXISTS "t_password_policy_pkey";
ALTER TABLE "t_password_policy" ADD CONSTRAINT "t_password_policy_pkey" PRIMARY KEY ("policy_id");
ALTER TABLE "t_password_policy" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_password_policy_id_seq" CASCADE;

-- ----------------------------
-- 18. t_update_password_remind_log 表
-- ----------------------------
ALTER TABLE "t_update_password_remind_log" RENAME COLUMN "id" TO "remind_log_id";
ALTER TABLE "t_update_password_remind_log" ALTER COLUMN "remind_log_id" TYPE varchar(50);
ALTER TABLE "t_update_password_remind_log" ALTER COLUMN "remind_log_id" DROP DEFAULT;
ALTER TABLE "t_update_password_remind_log" DROP CONSTRAINT IF EXISTS "t_update_password_remind_log_pkey";
ALTER TABLE "t_update_password_remind_log" ADD CONSTRAINT "t_update_password_remind_log_pkey" PRIMARY KEY ("remind_log_id");
DROP SEQUENCE IF EXISTS "t_update_password_remind_log_id_seq" CASCADE;

-- ----------------------------
-- 19. t_mail_template 表
-- ----------------------------
ALTER TABLE "t_mail_template" DROP CONSTRAINT IF EXISTS "t_mail_template_pkey";
ALTER TABLE "t_mail_template" ADD CONSTRAINT "t_mail_template_pkey" PRIMARY KEY ("template_id");
ALTER TABLE "t_mail_template" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_mail_template_id_seq" CASCADE;

-- ----------------------------
-- 20. t_audit_log 表
-- ----------------------------
ALTER TABLE "t_audit_log" DROP CONSTRAINT IF EXISTS "t_audit_log_pkey";
ALTER TABLE "t_audit_log" ADD CONSTRAINT "t_audit_log_pkey" PRIMARY KEY ("audit_id");
ALTER TABLE "t_audit_log" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_audit_log_id_seq" CASCADE;

-- ----------------------------
-- 21. t_obj_change_log 表
-- ----------------------------
ALTER TABLE "t_obj_change_log" RENAME COLUMN "id" TO "change_log_id";
ALTER TABLE "t_obj_change_log" ALTER COLUMN "change_log_id" TYPE varchar(50);
ALTER TABLE "t_obj_change_log" ALTER COLUMN "change_log_id" DROP DEFAULT;
ALTER TABLE "t_obj_change_log" DROP CONSTRAINT IF EXISTS "t_obj_change_log_pkey";
ALTER TABLE "t_obj_change_log" ADD CONSTRAINT "t_obj_change_log_pkey" PRIMARY KEY ("change_log_id");
DROP SEQUENCE IF EXISTS "t_obj_change_log_id_seq" CASCADE;

-- ----------------------------
-- 22. t_permission_exp_template 表
-- ----------------------------
ALTER TABLE "t_permission_exp_template" DROP CONSTRAINT IF EXISTS "t_permission_exp_template_pkey";
ALTER TABLE "t_permission_exp_template" ADD CONSTRAINT "t_permission_exp_template_pkey" PRIMARY KEY ("template_id");
ALTER TABLE "t_permission_exp_template" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_permission_exp_template_id_seq" CASCADE;

-- ----------------------------
-- 23. t_chat_answer 表
-- ----------------------------
ALTER TABLE "t_chat_answer" DROP CONSTRAINT IF EXISTS "t_chat_answer_pkey";
ALTER TABLE "t_chat_answer" ADD CONSTRAINT "t_chat_answer_pkey" PRIMARY KEY ("answer_id");
ALTER TABLE "t_chat_answer" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_chat_answer_id_seq" CASCADE;

-- ----------------------------
-- 24. t_chat_history 表
-- ----------------------------
ALTER TABLE "t_chat_history" RENAME COLUMN "id" TO "chat_history_id";
ALTER TABLE "t_chat_history" ALTER COLUMN "chat_history_id" TYPE varchar(50);
ALTER TABLE "t_chat_history" ALTER COLUMN "chat_history_id" DROP DEFAULT;
ALTER TABLE "t_chat_history" DROP CONSTRAINT IF EXISTS "t_chat_history_pkey";
ALTER TABLE "t_chat_history" ADD CONSTRAINT "t_chat_history_pkey" PRIMARY KEY ("chat_history_id");
DROP SEQUENCE IF EXISTS "t_chat_history_id_seq" CASCADE;

-- ----------------------------
-- 25. t_chat_message_history 表
-- ----------------------------
ALTER TABLE "t_chat_message_history" DROP CONSTRAINT IF EXISTS "t_chat_message_history_pkey";
ALTER TABLE "t_chat_message_history" ADD CONSTRAINT "t_chat_message_history_pkey" PRIMARY KEY ("message_id");
ALTER TABLE "t_chat_message_history" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_chat_message_history_id_seq" CASCADE;

-- ----------------------------
-- 26. t_data_source_conf 表
-- ----------------------------
ALTER TABLE "t_data_source_conf" DROP CONSTRAINT IF EXISTS "t_data_source_conf_pkey";
ALTER TABLE "t_data_source_conf" ADD CONSTRAINT "t_data_source_conf_pkey" PRIMARY KEY ("data_source_id");
ALTER TABLE "t_data_source_conf" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_data_source_conf_id_seq" CASCADE;

-- ----------------------------
-- 27. t_model_provider 表
-- ----------------------------
ALTER TABLE "t_model_provider" DROP CONSTRAINT IF EXISTS "t_model_provider_pkey";
ALTER TABLE "t_model_provider" ADD CONSTRAINT "t_model_provider_pkey" PRIMARY KEY ("provider_id");
ALTER TABLE "t_model_provider" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_model_provider_id_seq" CASCADE;

-- ----------------------------
-- 28. t_multi_chat_memory 表
-- ----------------------------
ALTER TABLE "t_multi_chat_memory" RENAME COLUMN "id" TO "memory_id";
ALTER TABLE "t_multi_chat_memory" ALTER COLUMN "memory_id" TYPE varchar(50);
ALTER TABLE "t_multi_chat_memory" ALTER COLUMN "memory_id" DROP DEFAULT;
ALTER TABLE "t_multi_chat_memory" DROP CONSTRAINT IF EXISTS "t_multi_chat_memory_pkey";
ALTER TABLE "t_multi_chat_memory" ADD CONSTRAINT "t_multi_chat_memory_pkey" PRIMARY KEY ("memory_id");
DROP SEQUENCE IF EXISTS "t_multi_chat_memory_id_seq" CASCADE;

-- ----------------------------
-- 29. t_table_field 表
-- ----------------------------
ALTER TABLE "t_table_field" DROP CONSTRAINT IF EXISTS "t_table_field_pkey";
ALTER TABLE "t_table_field" ADD CONSTRAINT "t_table_field_pkey" PRIMARY KEY ("field_id");
ALTER TABLE "t_table_field" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_table_field_id_seq" CASCADE;

-- ----------------------------
-- 30. t_table 表
-- ----------------------------
ALTER TABLE "t_table" DROP CONSTRAINT IF EXISTS "t_table_pkey";
ALTER TABLE "t_table" ADD CONSTRAINT "t_table_pkey" PRIMARY KEY ("table_id");
ALTER TABLE "t_table" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_table_id_seq" CASCADE;

-- ----------------------------
-- 31. t_webauthn_credential 表
-- ----------------------------
ALTER TABLE "t_webauthn_credential" DROP CONSTRAINT IF EXISTS "t_webauthn_credential_pkey";
ALTER TABLE "t_webauthn_credential" ADD CONSTRAINT "t_webauthn_credential_pkey" PRIMARY KEY ("credential_id");
ALTER TABLE "t_webauthn_credential" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_webauthn_credential_id_seq" CASCADE;

-- ----------------------------
-- 32. t_login_log 表
-- ----------------------------
ALTER TABLE "t_login_log" DROP CONSTRAINT IF EXISTS "t_login_log_pkey";
ALTER TABLE "t_login_log" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_login_log_id_seq" CASCADE;

-- ----------------------------
-- 33. t_dict 表
-- ----------------------------
ALTER TABLE "t_dict" DROP CONSTRAINT IF EXISTS "t_dict_pkey";
ALTER TABLE "t_dict" ADD CONSTRAINT "t_dict_pkey" PRIMARY KEY ("dict_id");
ALTER TABLE "t_dict" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_dict_id_seq" CASCADE;

-- ----------------------------
-- 34. t_dict_data 表
-- ----------------------------
ALTER TABLE "t_dict_data" DROP CONSTRAINT IF EXISTS "t_dict_data_pkey";
ALTER TABLE "t_dict_data" ADD CONSTRAINT "t_dict_data_pkey" PRIMARY KEY ("data_id");
ALTER TABLE "t_dict_data" DROP COLUMN IF EXISTS "id";
DROP SEQUENCE IF EXISTS "t_dict_data_id_seq" CASCADE;

-- =====================================================
-- 第二部分：添加索引优化查询性能
-- =====================================================

-- t_authorize 表索引
CREATE INDEX IF NOT EXISTS "idx_t_authorize_user_id" ON "t_authorize" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_authorize_role_id" ON "t_authorize" ("role_id");
CREATE INDEX IF NOT EXISTS "idx_t_authorize_user_group_id" ON "t_authorize" ("user_group_id");
CREATE INDEX IF NOT EXISTS "idx_t_authorize_permission_id" ON "t_authorize" ("permission_id");
CREATE INDEX IF NOT EXISTS "idx_t_authorize_time" ON "t_authorize" ("authorize_time");

-- t_role_mapping 表索引
CREATE INDEX IF NOT EXISTS "idx_t_role_mapping_user_id" ON "t_role_mapping" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_role_mapping_role_id" ON "t_role_mapping" ("role_id");
CREATE INDEX IF NOT EXISTS "idx_t_role_mapping_user_group_id" ON "t_role_mapping" ("user_group_id");

-- t_user_group_mapping 表索引
CREATE INDEX IF NOT EXISTS "idx_t_user_group_mapping_user_id" ON "t_user_group_mapping" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_user_group_mapping_group_id" ON "t_user_group_mapping" ("user_group_id");

-- t_user 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_user_username" ON "t_user" ("username");
CREATE INDEX IF NOT EXISTS "idx_t_user_email" ON "t_user" ("email_address");
CREATE INDEX IF NOT EXISTS "idx_t_user_phone" ON "t_user" ("phone_number");
CREATE INDEX IF NOT EXISTS "idx_t_user_deleted_create" ON "t_user" ("deleted", "create_time");

-- t_permission 表索引
CREATE INDEX IF NOT EXISTS "idx_t_permission_resource_id" ON "t_permission" ("resource_id");
CREATE INDEX IF NOT EXISTS "idx_t_permission_deleted" ON "t_permission" ("deleted");

-- t_resource 表索引
CREATE INDEX IF NOT EXISTS "idx_t_resource_group_id" ON "t_resource" ("resource_group_id");
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_resource_code" ON "t_resource" ("resource_code");
CREATE INDEX IF NOT EXISTS "idx_t_resource_api" ON "t_resource" ("api_identifier");
CREATE INDEX IF NOT EXISTS "idx_t_resource_deleted" ON "t_resource" ("deleted");

-- t_resource_group 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_resource_group_code" ON "t_resource_group" ("resource_group_code");
CREATE INDEX IF NOT EXISTS "idx_t_resource_group_deleted" ON "t_resource_group" ("deleted");

-- t_role 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_role_code" ON "t_role" ("role_code");
CREATE INDEX IF NOT EXISTS "idx_t_role_deleted" ON "t_role" ("deleted");

-- t_user_attr_mapping 表索引
CREATE INDEX IF NOT EXISTS "idx_t_user_attr_mapping_user_id" ON "t_user_attr_mapping" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_user_attr_mapping_attr_id" ON "t_user_attr_mapping" ("attr_id");

-- t_authorize_cond 表索引
CREATE INDEX IF NOT EXISTS "idx_t_authorize_cond_auth_id" ON "t_authorize_cond" ("authorize_id");
CREATE INDEX IF NOT EXISTS "idx_t_authorize_cond_exp_id" ON "t_authorize_cond" ("permission_exp_id");

-- t_oidc_claim_scope_mapping 表索引
CREATE INDEX IF NOT EXISTS "idx_t_oidc_claim_scope_claim_id" ON "t_oidc_claim_scope_mapping" ("claim_id");
CREATE INDEX IF NOT EXISTS "idx_t_oidc_claim_scope_scope_id" ON "t_oidc_claim_scope_mapping" ("scope_id");

-- t_permission_exp 表索引
CREATE INDEX IF NOT EXISTS "idx_t_permission_exp_deleted" ON "t_permission_exp" ("deleted");

-- t_user_attr 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_user_attr_key" ON "t_user_attr" ("attr_key");

-- t_user_group 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_user_group_code" ON "t_user_group" ("user_group_code");
CREATE INDEX IF NOT EXISTS "idx_t_user_group_deleted" ON "t_user_group" ("deleted");

-- t_client 表索引
CREATE INDEX IF NOT EXISTS "idx_t_client_deleted" ON "t_client" ("deleted");

-- t_login_log 表索引
CREATE INDEX IF NOT EXISTS "idx_t_login_log_user_id" ON "t_login_log" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_login_log_login_time" ON "t_login_log" ("login_time");

-- t_password_policy_mapping 表索引
CREATE INDEX IF NOT EXISTS "idx_t_password_policy_mapping_user_id" ON "t_password_policy_mapping" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_password_policy_mapping_policy_id" ON "t_password_policy_mapping" ("policy_id");

-- t_identity_source_provider 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_identity_source_provider_code" ON "t_identity_source_provider" ("provider_code");
CREATE INDEX IF NOT EXISTS "idx_t_identity_source_provider_deleted" ON "t_identity_source_provider" ("deleted");

-- t_identity_source_registration 表索引
CREATE INDEX IF NOT EXISTS "idx_t_identity_source_registration_provider_id" ON "t_identity_source_registration" ("provider_id");
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_identity_source_registration_code" ON "t_identity_source_registration" ("registration_code");
CREATE INDEX IF NOT EXISTS "idx_t_identity_source_registration_deleted" ON "t_identity_source_registration" ("deleted");

-- t_third_account 表索引
CREATE INDEX IF NOT EXISTS "idx_t_third_account_user_id" ON "t_third_account" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_third_account_registration_id" ON "t_third_account" ("registration_id");

-- t_password_policy 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_password_policy_code" ON "t_password_policy" ("policy_id");
CREATE INDEX IF NOT EXISTS "idx_t_password_policy_deleted" ON "t_password_policy" ("deleted");

-- t_mail_template 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_mail_template_code" ON "t_mail_template" ("template_code");
CREATE INDEX IF NOT EXISTS "idx_t_mail_template_deleted" ON "t_mail_template" ("deleted");

-- t_audit_log 表索引
CREATE INDEX IF NOT EXISTS "idx_t_audit_log_user_id" ON "t_audit_log" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_audit_log_audit_type" ON "t_audit_log" ("audit_type");
CREATE INDEX IF NOT EXISTS "idx_t_audit_log_operation_time" ON "t_audit_log" ("operation_time");

-- t_obj_change_log 表索引
CREATE INDEX IF NOT EXISTS "idx_t_obj_change_log_audit_id" ON "t_obj_change_log" ("audit_id");

-- t_permission_exp_template 表索引
CREATE INDEX IF NOT EXISTS "idx_t_permission_exp_template_deleted" ON "t_permission_exp_template" ("deleted");

-- t_chat_answer 表索引
CREATE INDEX IF NOT EXISTS "idx_t_chat_answer_chat_id" ON "t_chat_answer" ("chat_id");
CREATE INDEX IF NOT EXISTS "idx_t_chat_answer_create_time" ON "t_chat_answer" ("create_time");

-- t_chat_history 表索引
CREATE INDEX IF NOT EXISTS "idx_t_chat_history_user_id" ON "t_chat_history" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_chat_history_create_time" ON "t_chat_history" ("start_time");

-- t_chat_message_history 表索引
CREATE INDEX IF NOT EXISTS "idx_t_chat_message_history_chat_id" ON "t_chat_message_history" ("chat_id");
CREATE INDEX IF NOT EXISTS "idx_t_chat_message_history_question_id" ON "t_chat_message_history" ("question_id");
CREATE INDEX IF NOT EXISTS "idx_t_chat_message_history_user_id" ON "t_chat_message_history" ("user_id");
CREATE INDEX IF NOT EXISTS "idx_t_chat_message_history_create_time" ON "t_chat_message_history" ("create_time");

-- t_data_source_conf 表索引
CREATE INDEX IF NOT EXISTS "idx_t_data_source_conf_deleted" ON "t_data_source_conf" ("deleted");

-- t_model_provider 表索引
CREATE INDEX IF NOT EXISTS "idx_t_model_provider_deleted" ON "t_model_provider" ("deleted");

-- t_multi_chat_memory 表索引
CREATE INDEX IF NOT EXISTS "idx_t_multi_chat_memory_chat_id" ON "t_multi_chat_memory" ("chat_id");

-- t_table_field 表索引
CREATE INDEX IF NOT EXISTS "idx_t_table_field_table_id" ON "t_table_field" ("table_id");
CREATE INDEX IF NOT EXISTS "idx_t_table_field_deleted" ON "t_table_field" ("deleted");

-- t_table 表索引
CREATE INDEX IF NOT EXISTS "idx_t_table_data_source_id" ON "t_table" ("data_source_id");
CREATE INDEX IF NOT EXISTS "idx_t_table_deleted" ON "t_table" ("deleted");

-- t_webauthn_credential 表索引
CREATE INDEX IF NOT EXISTS "idx_t_webauthn_credential_user_id" ON "t_webauthn_credential" ("user_id");

-- t_dict 表索引
CREATE UNIQUE INDEX IF NOT EXISTS "idx_t_dict_code" ON "t_dict" ("dict_code");
CREATE INDEX IF NOT EXISTS "idx_t_dict_deleted" ON "t_dict" ("deleted");

-- t_dict_data 表索引
CREATE INDEX IF NOT EXISTS "idx_t_dict_data_dict_id" ON "t_dict_data" ("dict_id");
CREATE INDEX IF NOT EXISTS "idx_t_dict_data_deleted" ON "t_dict_data" ("deleted");
