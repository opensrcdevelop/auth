/**
 * 变更：
 *      1. 添加租户表 t_tenant
 *      2. 添加资源权限记录等
 */

-- ----------------------------
-- Table structure for t_tenant
-- ----------------------------
DROP TABLE IF EXISTS "t_tenant";

-- ----------------------------
-- Sequence structure for t_tenant_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_tenant_id_seq";
CREATE SEQUENCE "t_tenant_id_seq"
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

-- ----------------------------
-- Table structure for t_tenant
-- ----------------------------
DROP TABLE IF EXISTS "t_tenant";
CREATE TABLE "t_tenant" (
  "id" int8 NOT NULL DEFAULT nextval('t_tenant_id_seq'::regclass),
  "tenant_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "tenant_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "tenant_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "enabled" bool DEFAULT true,
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Uniques structure for table t_tenant
-- ----------------------------
ALTER TABLE "t_tenant" ADD CONSTRAINT "t_tenant_tenant_code_key" UNIQUE ("tenant_code");

-- ----------------------------
-- Primary Key structure for table t_tenant
-- ----------------------------
ALTER TABLE "t_tenant" ADD CONSTRAINT "t_tenant_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Records of t_resource
-- ----------------------------
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('055a8115-8be2-498e-a6f9-ab8a34f5dd0e', '多租户', 'tenant', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/tenant', NULL, '2024-09-01 15:27:32.970904', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_permission
-- ----------------------------
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('549ba99f-9686-4610-b92c-1355935ed145', '创建租户', 'create', NULL, '055a8115-8be2-498e-a6f9-ab8a34f5dd0e', '2024-09-01 15:29:12.981617', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('745edd90-ab69-4eb9-bd58-22b8f7c0eb64', '获取租户列表', 'list', NULL, '055a8115-8be2-498e-a6f9-ab8a34f5dd0e', '2024-09-01 15:29:32.297124', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('90e7cbfd-f28f-4408-be96-fb7834daeac0', '更新租户', 'update', NULL, '055a8115-8be2-498e-a6f9-ab8a34f5dd0e', '2024-09-01 15:32:32.765752', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('11d83c03-9d54-409f-a922-d92346182407', '删除租户', 'delete', NULL, '055a8115-8be2-498e-a6f9-ab8a34f5dd0e', '2024-09-01 15:43:41.427973', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('6a14da0a-b5b0-4876-b1ab-74b0555df7a1', '获取租户详情', 'detail', NULL, '055a8115-8be2-498e-a6f9-ab8a34f5dd0e', '2024-09-01 15:44:05.708854', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('55ce906d-3c48-45f3-81f2-e978a6f61cec', '所有权限', 'all', NULL, '055a8115-8be2-498e-a6f9-ab8a34f5dd0e', '2024-09-01 16:17:12.381848', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_authorize
-- ----------------------------
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '55ce906d-3c48-45f3-81f2-e978a6f61cec', 'ee8c2f05-6761-4551-98b4-dd0f64bfd0dc', '2024-09-01 16:18:46.434414');
