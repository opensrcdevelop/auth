/**
 * 变更：
 *      1. 添加表：【t_dict】、【t_dict_data】
 *      2. 表【t_user_attr】添加字段：【dict_id】
 *      3. 表【t_user_attr】添加数据
 *      4. 表【t_resource】添加数据
 *      5. 表【t_permission】添加数据
 *      6. 表【t_authorize】添加数据
 *
 */

-- ----------------------------
-- Sequence structure for t_dict_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_dict_id_seq";
CREATE SEQUENCE "t_dict_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

-- ----------------------------
-- Table structure for t_dict
-- ----------------------------
DROP TABLE IF EXISTS "t_dict";
CREATE TABLE "t_dict" (
  "id" int8 NOT NULL DEFAULT nextval('t_dict_id_seq'::regclass),
  "dict_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "dict_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "dict_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_dict"."id" IS '主键';
COMMENT ON COLUMN "t_dict"."dict_id" IS '字典ID';
COMMENT ON COLUMN "t_dict"."dict_code" IS '字典标识';
COMMENT ON COLUMN "t_dict"."dict_name" IS '字典名称';
COMMENT ON COLUMN "t_dict"."description" IS '描述';

-- ----------------------------
-- Uniques structure for table t_dict
-- ----------------------------
ALTER TABLE "t_dict" ADD CONSTRAINT "t_dict_dict_code_key" UNIQUE ("dict_code");
ALTER TABLE "t_dict" ADD CONSTRAINT "t_dict_dict_id_key" UNIQUE ("dict_id");

-- ----------------------------
-- Primary Key structure for table t_dict
-- ----------------------------
ALTER TABLE "t_dict" ADD CONSTRAINT "t_dict_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Records of t_dict
-- ----------------------------
INSERT INTO "t_dict" ("dict_id", "dict_code", "dict_name", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f327873b-d062-4332-bf99-cb36605932b7', 'dict_sex', '性别', NULL, '2024-11-17 10:59:41.0341', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Sequence structure for t_dict_data_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_dict_data_id_seq";
CREATE SEQUENCE "t_dict_data_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

-- ----------------------------
-- Table structure for t_dict_data
-- ----------------------------
DROP TABLE IF EXISTS "t_dict_data";
CREATE TABLE "t_dict_data" (
  "id" int8 NOT NULL DEFAULT nextval('t_dict_data_id_seq'::regclass),
  "data_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "dict_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "data_label" varchar(500) COLLATE "pg_catalog"."default" NOT NULL,
  "data_value" varchar(500) COLLATE "pg_catalog"."default" NOT NULL,
  "enable" bool DEFAULT true,
  "display_seq" int4,
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_dict_data"."id" IS '主键';
COMMENT ON COLUMN "t_dict_data"."data_id" IS '字典数据ID';
COMMENT ON COLUMN "t_dict_data"."dict_id" IS '字典ID';
COMMENT ON COLUMN "t_dict_data"."data_label" IS '字典数据标签';
COMMENT ON COLUMN "t_dict_data"."data_value" IS '字典数据值';
COMMENT ON COLUMN "t_dict_data"."enable" IS '是否启用';
COMMENT ON COLUMN "t_dict_data"."display_seq" IS '显示顺序';

-- ----------------------------
-- Uniques structure for table t_dict_data
-- ----------------------------
ALTER TABLE "t_dict_data" ADD CONSTRAINT "t_dict_data_data_id_key" UNIQUE ("data_id");

-- ----------------------------
-- Primary Key structure for table t_dict_data
-- ----------------------------
ALTER TABLE "t_dict_data" ADD CONSTRAINT "t_dict_data_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Records of t_dict_data
-- ----------------------------
INSERT INTO "t_dict_data" ("data_id", "dict_id", "data_label", "data_value", "enable", "display_seq", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('778dac90-f1bc-4d5f-bc13-721ec5edae93', 'f327873b-d062-4332-bf99-cb36605932b7', '女', 'f', 't', 2, '2024-11-17 11:00:35.980192', NULL, NULL, 'admin', 1, 'f');
INSERT INTO "t_dict_data" ("data_id", "dict_id", "data_label", "data_value", "enable", "display_seq", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b323ebd8-5a55-47b0-9d0d-ef64cfccccfb', 'f327873b-d062-4332-bf99-cb36605932b7', '男', 'm', 't', 1, '2024-11-17 11:00:12.533212', NULL, NULL, 'admin', 1, 'f');

ALTER TABLE "t_user_attr" ADD COLUMN "dict_id" varchar(50);
COMMENT ON COLUMN "t_user_attr"."dict_id" IS '字典ID';

INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width", "user_visible", "user_editable", "dict_id") VALUES ('8f8b5456-45bb-46f7-a267-95a531cc5edf', 'sex', '性别', 'DICT', 't', 'f', NULL, NULL, 't', 't', 'f327873b-d062-4332-bf99-cb36605932b7');

INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('22c26616-ba71-4258-a9b4-0901cc3285b7', '字典', 'dict', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/dict', NULL, '2024-11-19 22:32:36.562202', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ab15084d-285e-4a2d-bde2-d968a504e7a5', '字典数据', 'dictData', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/dict/data', NULL, '2024-11-19 22:33:25.681135', 'admin', NULL, NULL, 1, 'f');

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('64f679ec-66e0-4710-af85-f5e16742b043', '删除字典数据', 'delete', NULL, 'ab15084d-285e-4a2d-bde2-d968a504e7a5', '2024-11-19 22:46:45.355863', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('1d9efb1e-984c-4d12-b79e-9b677939df07', '获取字典数据列表', 'list', NULL, 'ab15084d-285e-4a2d-bde2-d968a504e7a5', '2024-11-19 22:45:30.357356', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('6a16982f-1f2f-45be-8cfe-5ebb64a3ee1d', '获取字典数据详情', 'detail', NULL, 'ab15084d-285e-4a2d-bde2-d968a504e7a5', '2024-11-19 22:44:36.549182', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ab2e6441-3314-418c-a93c-568d0d39cec0', '更新字典数据', 'update', NULL, 'ab15084d-285e-4a2d-bde2-d968a504e7a5', '2024-11-19 22:43:15.359421', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b5d2ed0e-617b-4364-835e-3ff1b44fa1cb', '创建字典数据', 'create', NULL, 'ab15084d-285e-4a2d-bde2-d968a504e7a5', '2024-11-19 22:42:06.143525', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('2b5c8c29-d961-4e40-9494-a50425bf50be', '所有权限', 'all', NULL, 'ab15084d-285e-4a2d-bde2-d968a504e7a5', '2024-11-19 22:41:15.507232', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b3b66866-8c7b-4695-a3b0-81d249fb32a4', '删除字典', 'delete', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2024-11-19 22:40:38.429164', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('11f0c32b-6fab-40b4-a664-9cc489898bc9', '获取字典列表', 'list', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2024-11-19 22:39:36.931788', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ea0e96b3-75fd-47ae-820a-639cfd403275', '获取字典详情', 'detail', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2024-11-19 22:38:37.089906', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5852b353-7794-4e5d-8536-42923760490d', '更新字典', 'update', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2024-11-19 22:37:43.274593', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('d42ccc60-6c7f-4b91-b125-27d7de645f5d', '创建字典', 'create', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2024-11-19 22:37:28.791222', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b1b58dab-2140-4f14-8d7c-6ca18f3d3441', '所有权限', 'all', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2024-11-19 22:34:52.244418', 'admin', NULL, NULL, 1, 'f');

INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'b1b58dab-2140-4f14-8d7c-6ca18f3d3441', 'e792813d-a112-4549-85b7-abd4ac3a2157', '2024-11-19 22:48:13.273688');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '2b5c8c29-d961-4e40-9494-a50425bf50be', '0698e4fc-5598-499c-8343-c248abecef55', '2024-11-19 22:48:18.869908');
