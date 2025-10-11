/**
 * 变更：
 *      1. 创建表 t_chat_answer t_chat_history t_chat_message_history t_data_source_conf t_model_provider t_multi_chat_memory t_table_field t_table
 *      2. 表【t_resource、t_permission、t_authorize】添加数据
 *
 */

-- ----------------------------
-- Table structure for t_chat_answer
-- ----------------------------
DROP TABLE IF EXISTS "t_chat_answer";
DROP SEQUENCE IF EXISTS "t_chat_answer_id_seq";
CREATE SEQUENCE "t_chat_answer_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_chat_answer" (
  "id" int8 NOT NULL DEFAULT nextval('t_chat_answer_id_seq'::regclass),
  "answer_id" varchar(50) NOT NULL,
  "data_source_id" varchar(50),
  "chat_id" varchar(50),
  "question_id" varchar(50),
  "question" text,
  "model_provider_id" varchar(50),
  "model" varchar(500),
  "chart_config" text,
  "sql" text,
  "report_type" varchar(100),
  "report" text,
  "answer" text,
  "feedback" varchar(500),
  "req_tokens" int4,
  "rep_tokens" int4,
  "create_by" varchar(255),
  "create_time" timestamp(6),
  "update_by" varchar(255),
  "update_time" timestamp(6),
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Primary Key structure for table t_chat_answer
-- ----------------------------
ALTER TABLE "t_chat_answer" ADD CONSTRAINT "t_chat_answer_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table structure for t_chat_history
-- ----------------------------
DROP TABLE IF EXISTS "t_chat_history";
DROP SEQUENCE IF EXISTS "t_chat_history_id_seq";
CREATE SEQUENCE "t_chat_history_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_chat_history" (
  "id" int8 NOT NULL DEFAULT nextval('t_chat_history_id_seq'::regclass),
  "chat_id" varchar(50),
  "user_id" varchar(50),
  "title" varchar(1000),
  "description" varchar(1000),
  "start_time" timestamp(6),
  "end_time" timestamp(6),
  "data_source_id" varchar(50)
)
;

-- ----------------------------
-- Primary Key structure for table t_chat_history
-- ----------------------------
ALTER TABLE "t_chat_history" ADD CONSTRAINT "t_chat_history_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table structure for t_chat_message_history
-- ----------------------------
DROP TABLE IF EXISTS "t_chat_message_history";
DROP SEQUENCE IF EXISTS "t_chat_message_history_id_seq";
CREATE SEQUENCE "t_chat_message_history_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_chat_message_history" (
  "id" int8 NOT NULL DEFAULT nextval('t_chat_message_history_id_seq'::regclass),
  "message_id" varchar(50),
  "role" varchar(255),
  "type" varchar(255),
  "content" text,
  "question_id" varchar(50),
  "chat_id" varchar(50),
  "rewritten_question" text,
  "user_id" varchar(50),
  "create_time" timestamp(6),
  "time" timestamp(6),
  "answer_id" varchar(50)
)
;

-- ----------------------------
-- Primary Key structure for table t_chat_message_history
-- ----------------------------
ALTER TABLE "t_chat_message_history" ADD CONSTRAINT "t_chat_message_history_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table structure for t_data_source_conf
-- ----------------------------
DROP TABLE IF EXISTS "t_data_source_conf";
DROP SEQUENCE IF EXISTS "t_data_source_conf_id_seq";
CREATE SEQUENCE "t_data_source_conf_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

DROP TABLE IF EXISTS "t_data_source_conf";
CREATE TABLE "t_data_source_conf" (
  "id" int8 NOT NULL DEFAULT nextval('t_data_source_conf_id_seq'::regclass),
  "data_source_id" varchar(50) NOT NULL,
  "data_source_name" varchar(255),
  "data_source_type" varchar(255),
  "database" varchar(255),
  "host" varchar(255),
  "port" int4,
  "username" varchar(255),
  "password" varchar(255),
  "enabled" bool,
  "last_sync_table_time" timestamp(6),
  "sync_table_count" int8,
  "system_ds" bool,
  "description" varchar(1000),
  "schema" varchar(255),
  "jdbc_params" text,
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Primary Key structure for table t_data_source_conf
-- ----------------------------
ALTER TABLE "t_data_source_conf" ADD CONSTRAINT "t_data_source_conf_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Records of t_data_source_conf
-- ----------------------------
INSERT INTO "t_data_source_conf" ("data_source_id", "data_source_name", "data_source_type", "database", "host", "port", "username", "password", "enabled", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "last_sync_table_time", "sync_table_count", "system_ds", "description", "schema", "jdbc_params") VALUES ('27058042-ae53-49cd-bd19-64a6e986f179', 'auth_server_ds', 'POSTGRESQL', '', '', NULL, '', '', 't', '2025-10-02 22:22:16.710267', NULL, NULL, NULL, 1, 'f', NULL, 0, 't', '系统数据源', NULL, NULL);

-- ----------------------------
-- Table structure for t_model_provider
-- ----------------------------
DROP TABLE IF EXISTS "t_model_provider";
DROP SEQUENCE IF EXISTS "t_model_provider_id_seq";
CREATE SEQUENCE "t_model_provider_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_model_provider" (
  "id" int8 NOT NULL DEFAULT nextval('t_model_provider_id_seq'::regclass),
  "provider_id" varchar(50) NOT NULL,
  "provider_name" varchar(255),
  "provider_type" varchar(255),
  "base_url" varchar(255),
  "api_key" varchar(255),
  "optional_models" varchar(500),
  "default_model" varchar(255),
  "temperature" varchar(255),
  "max_tokens" int4,
  "enabled" bool,
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Primary Key structure for table t_model_provider
-- ----------------------------
ALTER TABLE "t_model_provider" ADD CONSTRAINT "t_model_provider_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table structure for t_multi_chat_memory
-- ----------------------------
DROP TABLE IF EXISTS "t_multi_chat_memory";
DROP SEQUENCE IF EXISTS "t_multi_chat_memory_id_seq";
CREATE SEQUENCE "t_multi_chat_memory_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_multi_chat_memory" (
  "id" int8 NOT NULL DEFAULT nextval('t_multi_chat_memory_id_seq'::regclass),
  "chat_id" varchar(50),
  "prompt_template" varchar(255),
  "content" text,
  "type" varchar(255),
  "create_time" timestamp(6)
)
;

-- ----------------------------
-- Table structure for t_table_field
-- ----------------------------
DROP TABLE IF EXISTS "t_table_field";
DROP SEQUENCE IF EXISTS "t_table_field_id_seq";
CREATE SEQUENCE "t_table_field_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_table_field" (
  "id" int8 NOT NULL DEFAULT nextval('t_table_field_id_seq'::regclass),
  "table_id" varchar(50),
  "field_id" varchar(50) NOT NULL,
  "field_name" varchar(255),
  "field_type" varchar(255),
  "remark" text,
  "to_use" bool,
  "additional_info" text,
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Primary Key structure for table t_table_field
-- ----------------------------
ALTER TABLE "t_table_field" ADD CONSTRAINT "t_table_field_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table structure for t_table
-- ----------------------------
DROP TABLE IF EXISTS "t_table";
DROP SEQUENCE IF EXISTS "t_table_id_seq";
CREATE SEQUENCE "t_table_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_table" (
  "id" int8 NOT NULL DEFAULT nextval('t_table_id_seq'::regclass),
  "data_source_id" varchar(50),
  "table_id" varchar(50) NOT NULL,
  "table_name" varchar(255),
  "remark" text,
  "to_use" bool,
  "additional_info" text,
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Primary Key structure for table t_table
-- ----------------------------
ALTER TABLE "t_table" ADD CONSTRAINT "t_table_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Records of t_resource
-- ----------------------------
DELETE FROM "t_resource" WHERE "resource_id" = '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa';

INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', 'ChatBI问数', 'chatBI', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/chatbi', NULL, '2025-10-08 22:09:14.7385', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_permission
-- ----------------------------
DELETE FROM "t_permission" WHERE "permission_id" = '0199c428-d163-7076-94d4-11ebfb53b960';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c429-d571-7c28-9961-ac7d853e7fc6';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42a-5cfd-72b0-b5aa-0062ec9102d3';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42b-0725-7c49-ab44-65c1ac59ac0d';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42b-8225-78c4-b5db-25083f979997';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42b-ed2f-7b02-8c97-614814bdcc6e';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42c-30e8-76d0-bc74-05bc2af9fd47';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42c-aa87-768f-b63e-86ba0d40fb06';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42c-fc63-7c45-860b-d637c85b0a2d';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42d-3108-7bca-8b17-09942f9b4656';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42e-621a-7fa5-b527-cf4570c50c54';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42e-cc01-75a1-a1a9-d15861aec84b';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42f-326b-7e28-aa55-c69c308a635f';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42f-88c4-7122-a289-cb8c869548de';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c430-1534-7ac0-a832-3e167a1b2383';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c430-6bf1-7cf1-9d40-f369739ec8bf';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c430-b7ac-7b81-bbff-a8f0df4d67fc';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c43d-d736-7ff3-b43c-88ccf7a45ffd';

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c428-d163-7076-94d4-11ebfb53b960', '所有权限', 'all', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:10:33.443285', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c429-d571-7c28-9961-ac7d853e7fc6', '批量更新数据源表字段', 'batchUpdateDataSourceTableField', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:11:40.017888', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42a-5cfd-72b0-b5aa-0062ec9102d3', '批量更新数据源表', 'batchUpdateDataSourceTable', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:12:14.717259', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42b-0725-7c49-ab44-65c1ac59ac0d', '更新模型提供商', 'updateModelProvider', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:12:58.277896', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42b-8225-78c4-b5db-25083f979997', '创建模型提供商', 'createModelProvider', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:13:29.765626', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42b-ed2f-7b02-8c97-614814bdcc6e', '更新数据源配置', 'updateDataSourceConf', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:13:57.1678', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42c-30e8-76d0-bc74-05bc2af9fd47', '创建数据源配置', 'createDataSourceConf', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:14:14.504506', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42c-aa87-768f-b63e-86ba0d40fb06', '同步数据源表', 'syncDataSourceTable', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:14:45.639489', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42c-fc63-7c45-860b-d637c85b0a2d', '测试数据源连接', 'testDataSourceConn', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:15:06.595851', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42d-3108-7bca-8b17-09942f9b4656', '对话', 'chat', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:15:20.072829', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42e-621a-7fa5-b527-cf4570c50c54', '获取数据源表字段列表', 'getDataSourceTableFieldList', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:16:38.171087', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42e-cc01-75a1-a1a9-d15861aec84b', '获取模型提供商详情', 'getModelProviderDetail', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:17:05.281459', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42f-326b-7e28-aa55-c69c308a635f', '删除模型提供商', 'deleteModelProvider', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:17:31.499969', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c42f-88c4-7122-a289-cb8c869548de', '获取模型提供商列表', 'getModelProviderList', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:17:53.604148', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c430-1534-7ac0-a832-3e167a1b2383', '获取数据源配置详情', 'getDataSourceConfDetail', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:18:29.556766', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c430-6bf1-7cf1-9d40-f369739ec8bf', '删除数据源配置', 'deleteDataSourceConf', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:18:51.761889', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c430-b7ac-7b81-bbff-a8f0df4d67fc', '获取数据源表列表', 'getDataSourceTableList', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:19:11.148784', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c43d-d736-7ff3-b43c-88ccf7a45ffd', '获取数据源配置列表', 'getDataSourceConfList', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-10-08 22:33:31.191111', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_authorize
-- ----------------------------
DELETE FROM "t_authorize" WHERE "authorize_id" = '0199c431-fbfc-7a7e-83dc-276697438c85';

INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '0199c428-d163-7076-94d4-11ebfb53b960', '0199c431-fbfc-7a7e-83dc-276697438c85', '2025-10-08 22:20:34.172527', 0);
