/**
 * 权限申请与审批模块数据库表结构
 *
 * 变更说明：
 *     1. 创建表【t_permission_request】- 权限申请表
 *     2. 创建表【t_permission_request_item】- 权限申请明细表
 *     3. 表【t_permission】添加 allow_apply、auto_approve 字段
 */

-- ----------------------------
-- 表【t_permission】添加字段
-- ----------------------------
ALTER TABLE "t_permission" ADD COLUMN IF NOT EXISTS "allow_apply" bool NOT NULL DEFAULT true;
ALTER TABLE "t_permission" ADD COLUMN IF NOT EXISTS "auto_approve" bool NOT NULL DEFAULT false;

COMMENT ON COLUMN "t_permission"."allow_apply" IS '是否允许用户申请';
COMMENT ON COLUMN "t_permission"."auto_approve" IS '申请后是否自动批准';

-- ----------------------------
-- 表结构：t_permission_request（权限申请表）
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_request";

CREATE TABLE "t_permission_request" (
  "request_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "user_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "reason" varchar(500) COLLATE "pg_catalog"."default",
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
COMMENT ON COLUMN "t_permission_request"."request_id" IS '申请ID（UUID）';
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
  "item_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "request_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "permission_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "status" varchar(20) COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'PENDING',
  "reject_reason" varchar(500) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false,
  PRIMARY KEY ("item_id")
);

COMMENT ON TABLE "t_permission_request_item" IS '权限申请明细表';
COMMENT ON COLUMN "t_permission_request_item"."item_id" IS '申请明细ID（UUID）';
COMMENT ON COLUMN "t_permission_request_item"."request_id" IS '关联申请ID';
COMMENT ON COLUMN "t_permission_request_item"."permission_id" IS '申请的权限ID';
COMMENT ON COLUMN "t_permission_request_item"."status" IS '审批状态（PENDING待审批/APPROVED已批准/REJECTED已拒绝/AUTO_APPROVED自动批准）';
COMMENT ON COLUMN "t_permission_request_item"."reject_reason" IS '拒绝理由（针对单个权限，可选）';
COMMENT ON COLUMN "t_permission_request_item"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_permission_request_item"."create_by" IS '创建人';
COMMENT ON COLUMN "t_permission_request_item"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_permission_request_item"."update_by" IS '更新人';
COMMENT ON COLUMN "t_permission_request_item"."version" IS '版本';
COMMENT ON COLUMN "t_permission_request_item"."deleted" IS '逻辑删除标记';

-- ----------------------------
-- 删除表：t_permission_auto_approve（已由 t_permission.auto_approve 替代）
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_auto_approve";

-- ----------------------------
-- 索引：t_permission_request
-- ----------------------------
CREATE INDEX "idx_t_permission_request_user" ON "t_permission_request" ("user_id");

-- ----------------------------
-- 索引：t_permission_request_item
-- ----------------------------
CREATE INDEX "idx_t_permission_request_item_request" ON "t_permission_request_item" ("request_id");
CREATE INDEX "idx_t_permission_request_item_permission" ON "t_permission_request_item" ("permission_id");

-- ----------------------------
-- 外键约束
-- ----------------------------
ALTER TABLE "t_permission_request_item" ADD CONSTRAINT "fk_t_permission_request_item_request" FOREIGN KEY ("request_id") REFERENCES "t_permission_request" ("request_id");
