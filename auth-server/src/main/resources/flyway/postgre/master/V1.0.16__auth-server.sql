/**
 * 变更：
 *      1. 创建表【t_permission_exp_template】
 *      2. 修改表【t_permission_exp】
 *      2. 表【t_resource、t_permission、t_authorize】添加数据
 *
 */

-- ----------------------------
-- Table structure for t_permission_exp_template
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_exp_template";
DROP SEQUENCE IF EXISTS "t_permission_exp_template_id_seq";
CREATE SEQUENCE "t_permission_exp_template_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

DROP TABLE IF EXISTS "t_permission_exp_template";
CREATE TABLE "t_permission_exp_template" (
  "id" int8 NOT NULL DEFAULT nextval('t_permission_exp_template_id_seq'::regclass),
  "template_id" varchar(50) NOT NULL,
  "template_name" varchar(255),
  "expression" text,
  "template_param_configs" text,
  "description" varchar(500),
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Records of t_permission_exp_template
-- ----------------------------
BEGIN;
INSERT INTO "t_permission_exp_template" ("template_id", "template_name", "expression", "template_param_configs", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019813a6-1f89-7cf1-936c-4f1521a0448a', '限制请求 IP', 'var inRange = false;
for (cidr : cidrList) {
  if (fn_ip:isIpInCidr(_reqCtx.ip, cidr)) {
    inRange = true;
    break;
  }
}
if (cidrListType == ''黑名单'') {
  inRange = !inRange;
}
inRange;', '[{"type":"LIST","name":"CIDR 范围列表","code":"cidrList","required":true,"defaultValue":"10.0.0.0/8\n172.16.0.0/12\n192.168.0.0/16"},{"type":"CHOICE","name":"CIDR 范围列表类型","code":"cidrListType","options":["白名单","黑名单"],"multiple":false,"required":true,"defaultValue":"白名单"}]', '检查请求 IP 地址是否在 CIDR 范围内。', '2025-07-16 22:31:50.925025', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission_exp_template" ("template_id", "template_name", "expression", "template_param_configs", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019809a4-25e3-7374-9707-96a92d71c332', '限制请求时间', 'nowTime = fn_time:nowTime();
(startTime <= endTime && nowTime >= startTime && nowTime <= endTime) ||
(startTime > endTime && (nowTime >= startTime || nowTime <= endTime))', '[{"type":"STRING","name":"开始时间（HHmm）","code":"startTime","required":true,"defaultValue":"0000"},{"type":"STRING","name":"结束时间（HHmm）","code":"endTime","required":true,"defaultValue":"2359"}]', '检查请求时间是否在允许的范围内。', '2025-07-14 23:53:29.318613', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission_exp_template" ("template_id", "template_name", "expression", "template_param_configs", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019813fc-1a57-7b9d-b4b9-d50b741952bc', '限制请求设备类型', 'var inRange = false;
for (deviceType : deviceTypes) {
  if (_reqCtx.deviceType == deviceType) {
    inRange = true;
    break;
  }
}
inRange', '[{"type":"CHOICE","name":"设备类型","code":"deviceTypes","options":["Mobile Phone","Desktop","Tablet","Unknown"],"multiple":true,"required":true}]', '检查请求设备类型是否在允许的范围内。', '2025-07-17 00:05:45.689898', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission_exp_template" ("template_id", "template_name", "expression", "template_param_configs", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('01982d31-31bb-78b4-8b5c-4590534f2382', '限制请求浏览器类型', 'var inRange = false;
for (browserType : browserTypes) {
  if (_reqCtx.browserType == browserType) {
    inRange = true;
    break;
  }
}
inRange', '[{"type":"CHOICE","name":"浏览器类型","code":"browserTypes","desc":null,"options":["Chrome","Edge","Firefox","Unknown"],"multiple":true,"required":true,"defaultValue":null}]', '检查请求浏览器类型是否在允许的范围内。', '2025-07-21 21:34:15.484567', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission_exp_template" ("template_id", "template_name", "expression", "template_param_configs", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('01982d38-5ab8-7dc2-a033-28043f41244d', '限制请求设备 OS 类型', 'var inRange = false;
for (osType : osTypes) {
  if (_reqCtx.osType == osType) {
    inRange = true;
    break;
  }
}
inRange', '[{"type":"CHOICE","name":"请求设备 OS 类型","code":"osTypes","desc":null,"options":["Windows","macOS","Android","iOS","HarmonyOS","Unknown"],"multiple":true,"required":true,"defaultValue":null}]', '检查请求设备 OS 类型是否在允许的范围内。', '2025-07-21 21:42:04.729251', 'admin', NULL, NULL, 1, 'f');
COMMIT;

-- ----------------------------
-- Primary Key structure for table t_permission_exp_template
-- ----------------------------
ALTER TABLE "t_permission_exp_template" ADD CONSTRAINT "t_permission_exp_template_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Alter table t_permission_exp
-- ----------------------------
ALTER TABLE "t_permission_exp"
    DROP COLUMN IF EXISTS "template_id",
    DROP COLUMN IF EXISTS "template_params",
    ADD COLUMN "template_id" varchar(50),
    ADD COLUMN "template_params" text,
    ALTER COLUMN "expression" DROP NOT NULL,
    ALTER COLUMN "expression" TYPE text;

-- ----------------------------
-- Records of t_resource
-- ----------------------------
DELETE FROM "t_resource" WHERE "resource_id" = '01982839-ae11-75ae-8680-4bb5e0bbe83f';

INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('01982839-ae11-75ae-8680-4bb5e0bbe83f', '权限表达式模板', 'permissionExpTemplate', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/permission/exp/template', NULL, '2025-07-20 22:25:25.523621', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_permission
-- ----------------------------

DELETE FROM "t_permission" WHERE "permission_id" = '0198286b-2a92-715c-b881-74c9b40651ac';
DELETE FROM "t_permission" WHERE "permission_id" = '0198286b-6a92-793b-88a4-9349303e62e8';
DELETE FROM "t_permission" WHERE "permission_id" = '0198286b-9f2e-7bae-9c58-21dac82717b7';
DELETE FROM "t_permission" WHERE "permission_id" = '0198286b-d014-73e3-baaf-cadba0ff3048';
DELETE FROM "t_permission" WHERE "permission_id" = '0198286c-58d2-7800-8767-17e4e00575e1';
DELETE FROM "t_permission" WHERE "permission_id" = '0198286d-fb53-7c62-962c-1b83bc57720a';
DELETE FROM "t_permission" WHERE "permission_id" = '01982872-2bc4-7b99-aa83-74258264eb8a';
DELETE FROM "t_permission" WHERE "permission_id" = '01982872-9b5e-70f5-9d78-a10b7a8e5d7a';


INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0198286b-2a92-715c-b881-74c9b40651ac', '所有权限', 'all', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:19:28.658582', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0198286b-6a92-793b-88a4-9349303e62e8', '创建权限表达式模板', 'create', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:19:45.042689', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0198286b-9f2e-7bae-9c58-21dac82717b7', '更新权限表达式模板', 'update', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:19:58.510835', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0198286b-d014-73e3-baaf-cadba0ff3048', '删除权限表达式模板', 'delete', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:20:11.02835', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0198286c-58d2-7800-8767-17e4e00575e1', '获取权限表达式列表', 'list', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:20:46.034593', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0198286d-fb53-7c62-962c-1b83bc57720a', '获取权限表达式模板详情', 'detail', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:22:33.171869', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('01982872-2bc4-7b99-aa83-74258264eb8a', '获取权限表达式模板参数配置', 'paramConfigs', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:27:07.716878', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('01982872-9b5e-70f5-9d78-a10b7a8e5d7a', '获取关联的权限表达式列表', 'exps', NULL, '01982839-ae11-75ae-8680-4bb5e0bbe83f', '2025-07-20 23:27:36.286194', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_authorize
-- ----------------------------
DELETE FROM "t_authorize" WHERE "authorize_id" = '0198286f-70c0-7c62-865b-943dc1d116aa';

INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '0198286b-2a92-715c-b881-74c9b40651ac', '0198286f-70c0-7c62-865b-943dc1d116aa', '2025-07-20 23:24:08.768644', 0);
