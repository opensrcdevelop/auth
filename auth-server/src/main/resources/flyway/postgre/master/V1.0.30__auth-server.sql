/**
 * 权限申请与审批模块数据库表结构
 *
 * 变更说明：
 *     1. 创建表【t_permission_request】- 权限申请表
 *     2. 创建表【t_permission_request_item】- 权限申请明细表
 *     3. 表【t_permission】添加 allow_apply、auto_approve 字段
 *     4. 表【t_authorize】添加 authorizer_id、type 字段
 *     5. 添加资源、权限、授权
 */

-- ----------------------------
-- 表【t_permission】添加字段
-- ----------------------------
ALTER TABLE "t_permission" ADD COLUMN IF NOT EXISTS "allow_apply" bool NOT NULL DEFAULT true;
ALTER TABLE "t_permission" ADD COLUMN IF NOT EXISTS "auto_approve" bool NOT NULL DEFAULT false;

COMMENT ON COLUMN "t_permission"."allow_apply" IS '是否允许用户申请';
COMMENT ON COLUMN "t_permission"."auto_approve" IS '申请后是否自动批准';

-- ----------------------------
-- 表【t_authorize】添加字段
-- ----------------------------
ALTER TABLE "t_authorize" ADD COLUMN IF NOT EXISTS "authorizer_id" varchar(50);
ALTER TABLE "t_authorize" ADD COLUMN IF NOT EXISTS "type" varchar(255) DEFAULT 'SYSTEM_DEFAULT';

COMMENT ON COLUMN "t_authorize"."authorizer_id" IS '授权人ID';
COMMENT ON COLUMN "t_authorize"."type" IS '授权类型（SYSTEM_DEFAULT：系统默认、ADMINISTRATOR_GRANT：管理员赋予、ADMINISTRATOR_APPROVE：管理员审批、AUTO_APPROVE：自动审批）';


-- ----------------------------
-- 表结构：t_permission_request（权限申请表）
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_request" CASCADE;

CREATE TABLE "t_permission_request" (
  "request_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "user_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "reason" varchar(1000) COLLATE "pg_catalog"."default",
  "request_time" timestamp(6) NOT NULL,
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false,
  PRIMARY KEY ("request_id")
);

COMMENT ON TABLE "t_permission_request" IS '权限申请表';
COMMENT ON COLUMN "t_permission_request"."request_id" IS '申请ID';
COMMENT ON COLUMN "t_permission_request"."user_id" IS '申请人ID';
COMMENT ON COLUMN "t_permission_request"."reason" IS '申请理由';
COMMENT ON COLUMN "t_permission_request"."request_time" IS '申请时间';
COMMENT ON COLUMN "t_permission_request"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_permission_request"."create_by" IS '创建人';
COMMENT ON COLUMN "t_permission_request"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_permission_request"."update_by" IS '更新人';
COMMENT ON COLUMN "t_permission_request"."version" IS '版本';
COMMENT ON COLUMN "t_permission_request"."deleted" IS '逻辑删除标记';

-- ----------------------------
-- 表结构：t_permission_request_item（权限申请明细表）
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_request_item";

CREATE TABLE "t_permission_request_item" (
  "item_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "request_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "permission_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "status" varchar(20) COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'PENDING',
  "reject_reason" varchar(1000) COLLATE "pg_catalog"."default",
  "approver_id" varchar(50) COLLATE "pg_catalog"."default",
  "approve_time" timestamp(6),
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false,
  PRIMARY KEY ("item_id")
);

COMMENT ON TABLE "t_permission_request_item" IS '权限申请明细表';
COMMENT ON COLUMN "t_permission_request_item"."item_id" IS '申请明细ID';
COMMENT ON COLUMN "t_permission_request_item"."request_id" IS '关联申请ID';
COMMENT ON COLUMN "t_permission_request_item"."permission_id" IS '申请的权限ID';
COMMENT ON COLUMN "t_permission_request_item"."status" IS '审批状态（PENDING：待审批、APPROVED：已批准、REJECTED：已拒绝、AUTO_APPROVED：自动批准）';
COMMENT ON COLUMN "t_permission_request_item"."reject_reason" IS '拒绝理由';
COMMENT ON COLUMN "t_permission_request_item"."approver_id" IS '审批人ID';
COMMENT ON COLUMN "t_permission_request_item"."approve_time" IS '审批时间';
COMMENT ON COLUMN "t_permission_request_item"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_permission_request_item"."create_by" IS '创建人';
COMMENT ON COLUMN "t_permission_request_item"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_permission_request_item"."update_by" IS '更新人';
COMMENT ON COLUMN "t_permission_request_item"."version" IS '版本';
COMMENT ON COLUMN "t_permission_request_item"."deleted" IS '逻辑删除标记';

-- ----------------------------
-- 索引：t_permission_request_item
-- ----------------------------
CREATE INDEX "idx_t_permission_request_item_request" ON "t_permission_request_item" ("request_id");
CREATE INDEX "idx_t_permission_request_item_permission" ON "t_permission_request_item" ("permission_id");


-- ----------------------------
-- 添加资源、权限、授权
-- ----------------------------
DELETE FROM "t_resource" WHERE "resource_id" = '019d9c5e-9cef-7378-945d-d0eb6d2ded7b';
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019d9c5e-9cef-7378-945d-d0eb6d2ded7b', '权限申请', 'permissionRequest', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/permission/request', NULL, '2026-04-18 00:55:39.504158', 'admin', NULL, NULL, 1, 'f');

DELETE FROM "t_permission" WHERE "permission_id" = '019da4b6-1955-78c8-b8af-76572186e994';
DELETE FROM "t_permission" WHERE "permission_id" = '019da3d7-aab6-70a7-b485-1c9a471e77fc';
DELETE FROM "t_permission" WHERE "permission_id" = '019da12b-7acc-748b-aed4-193c9514e5f7';
DELETE FROM "t_permission" WHERE "permission_id" = '019da12a-a08a-73b6-a640-4db00db1ce57';
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "allow_apply", "auto_approve") VALUES ('019da4b6-1955-78c8-b8af-76572186e994', '所有权限', 'all', NULL, '019d9c5e-9cef-7378-945d-d0eb6d2ded7b', '2026-04-19 15:48:10.711382', 'admin', NULL, NULL, 1, 'f', 't', 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "allow_apply", "auto_approve") VALUES ('019da3d7-aab6-70a7-b485-1c9a471e77fc', '获取权限申请详情', 'detail', NULL, '019d9c5e-9cef-7378-945d-d0eb6d2ded7b', '2026-04-19 11:45:13.400565', 'admin', NULL, NULL, 1, 'f', 't', 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "allow_apply", "auto_approve") VALUES ('019da12b-7acc-748b-aed4-193c9514e5f7', '审批权限申请', 'approve', NULL, '019d9c5e-9cef-7378-945d-d0eb6d2ded7b', '2026-04-18 23:17:54.50888', 'admin', NULL, NULL, 1, 'f', 't', 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "allow_apply", "auto_approve") VALUES ('019da12a-a08a-73b6-a640-4db00db1ce57', '获取权限申请列表', 'list', NULL, '019d9c5e-9cef-7378-945d-d0eb6d2ded7b', '2026-04-18 23:16:58.635827', 'admin', NULL, NULL, 1, 'f', 't', 'f');

DELETE FROM "t_authorize" WHERE "authorize_id" = '019da4ba-a0a7-73fb-8ebc-bbf2b4b6c8aa';
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority", "authorizer_id", "type") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '019da4b6-1955-78c8-b8af-76572186e994', '019da4ba-a0a7-73fb-8ebc-bbf2b4b6c8aa', '2026-04-19 15:53:07.494443', 0, NULL, 'SYSTEM_DEFAULT');
