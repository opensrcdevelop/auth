/**
 * 变更：
 *      1. 创建表【t_audit_log】、【t_obj_change_log】
 *      2. 表【t_resource、t_permission、t_authorize】添加数据
 *
 */
 
 -- ----------------------------
-- Table structure for t_audit_log
-- ----------------------------
DROP TABLE IF EXISTS "t_audit_log";
DROP SEQUENCE IF EXISTS "t_audit_log_id_seq";
CREATE SEQUENCE "t_audit_log_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

CREATE TABLE "t_audit_log" (
  "id" int8 NOT NULL DEFAULT nextval('t_audit_log_id_seq'::regclass),
  "audit_id" varchar(50) NOT NULL,
  "user_id" varchar(50),
  "audit_type" int2,
  "operation_type" int2,
  "resource_id" varchar(50),
  "operation_result" bool,
  "operation_time" timestamp(6),
  "operation_detail" text,
  "ip" varchar(50),
  "ip_region" varchar(255),
  "device_type" varchar(255),
  "os_type" varchar(255),
  "browser_type" varchar(255),
  "extra_info" text,
  "request_id" varchar(255)
)
;

-- ----------------------------
-- Primary Key structure for table t_audit_log
-- ----------------------------
ALTER TABLE "t_audit_log" ADD CONSTRAINT "t_audit_log_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table structure for t_obj_change_log
-- ----------------------------
DROP TABLE IF EXISTS "t_obj_change_log";
DROP SEQUENCE IF EXISTS "t_obj_change_log_id_seq";
CREATE SEQUENCE "t_obj_change_log_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

CREATE TABLE "t_obj_change_log" (
  "id" int8 NOT NULL DEFAULT nextval('t_obj_change_log_id_seq'::regclass),
  "audit_id" varchar(50) NOT NULL,
  "java_type" varchar(500),
  "obj_id" varchar(500),
  "before" text,
  "after" text
)
;

-- ----------------------------
-- Primary Key structure for table t_obj_change_log
-- ----------------------------
ALTER TABLE "t_obj_change_log" ADD CONSTRAINT "t_obj_change_log_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Records of t_resource
-- ----------------------------
DELETE FROM "t_resource" WHERE "resource_id" = '0197bb1d-e669-773b-9075-96ea20aa51b4';

INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0197bb1d-e669-773b-9075-96ea20aa51b4', '审计日志', 'auditLog', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/auditLog', NULL, '2025-06-29 17:56:28.393839', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_permission
-- ----------------------------
DELETE FROM "t_permission" WHERE "permission_id" = '0197bb1e-3d2d-7b43-9095-519c1fba56fb';
DELETE FROM "t_permission" WHERE "permission_id" = '0197bb1e-cd14-75e7-a209-d8171e071413';
DELETE FROM "t_permission" WHERE "permission_id" = '0197bb1f-633a-7fdf-acb9-44dbb2239c27';
DELETE FROM "t_permission" WHERE "permission_id" = '0197bb1f-a610-7f5c-ad92-cf3f919c8bb2';

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0197bb1e-3d2d-7b43-9095-519c1fba56fb', '所有权限', 'all', NULL, '0197bb1d-e669-773b-9075-96ea20aa51b4', '2025-06-29 17:56:50.605966', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0197bb1e-cd14-75e7-a209-d8171e071413', '获取用户操作日志', 'userOperation', NULL, '0197bb1d-e669-773b-9075-96ea20aa51b4', '2025-06-29 17:57:27.44446', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0197bb1f-633a-7fdf-acb9-44dbb2239c27', '获取系统操作日志', 'sysOperation', NULL, '0197bb1d-e669-773b-9075-96ea20aa51b4', '2025-06-29 17:58:05.883177', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0197bb1f-a610-7f5c-ad92-cf3f919c8bb2', '获取对象变更日志', 'objChange', NULL, '0197bb1d-e669-773b-9075-96ea20aa51b4', '2025-06-29 17:58:22.993056', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_authorize
-- ----------------------------
DELETE FROM "t_authorize" WHERE "authorize_id" = '0197bb20-67d8-767e-912d-7af98cb27dfb';

INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '0197bb1e-3d2d-7b43-9095-519c1fba56fb', '0197bb20-67d8-767e-912d-7af98cb27dfb', '2025-06-29 17:59:12.60021', 0);
