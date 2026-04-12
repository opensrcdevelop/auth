/**
 * 权限申请与审批模块数据库表结构
 *
 * 变更说明：
 *     1. 创建表【t_permission_request】- 权限申请表
 *     2. 创建表【t_permission_request_item】- 权限申请明细表
 *     3. 创建表【t_permission_request_cond】- 权限审批限制条件关联表
 *     4. 创建表【t_permission_auto_approve】- 自动批准配置表
 *
 * 注意：本迁移在 per-tenant 数据库执行，不需要 tenant_id 字段
 */

-- ----------------------------
-- 表结构：t_permission_request（权限申请表）
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_request";

CREATE TABLE "t_permission_request" (
  "request_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "user_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "reason" varchar(500) COLLATE "pg_catalog"."default",
  "status" varchar(20) COLLATE "pg_catalog"."default" NOT NULL DEFAULT 'PENDING',
  "request_time" timestamp(6) NOT NULL,
  "approver_id" varchar(32) COLLATE "pg_catalog"."default",
  "approve_time" timestamp(6),
  "reject_reason" varchar(500) COLLATE "pg_catalog"."default",
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
COMMENT ON COLUMN "t_permission_request"."status" IS '申请状态（PENDING待审批/APPROVED已批准/REJECTED已拒绝/AUTO_APPROVED自动批准）';
COMMENT ON COLUMN "t_permission_request"."request_time" IS '申请时间';
COMMENT ON COLUMN "t_permission_request"."approver_id" IS '审批人ID';
COMMENT ON COLUMN "t_permission_request"."approve_time" IS '审批时间';
COMMENT ON COLUMN "t_permission_request"."reject_reason" IS '拒绝理由';
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
  "auto_approved" bool DEFAULT false,
  "restriction_ids" varchar(500) COLLATE "pg_catalog"."default",
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
COMMENT ON COLUMN "t_permission_request_item"."auto_approved" IS '是否自动批准';
COMMENT ON COLUMN "t_permission_request_item"."restriction_ids" IS '批准的限制条件ID列表（逗号分隔）';
COMMENT ON COLUMN "t_permission_request_item"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_permission_request_item"."create_by" IS '创建人';
COMMENT ON COLUMN "t_permission_request_item"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_permission_request_item"."update_by" IS '更新人';
COMMENT ON COLUMN "t_permission_request_item"."version" IS '版本';
COMMENT ON COLUMN "t_permission_request_item"."deleted" IS '逻辑删除标记';

-- ----------------------------
-- 表结构：t_permission_request_cond（权限审批限制条件关联表）
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_request_cond";

CREATE TABLE "t_permission_request_cond" (
  "cond_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "request_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "item_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "exp_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false,
  PRIMARY KEY ("cond_id")
);

COMMENT ON TABLE "t_permission_request_cond" IS '权限审批限制条件关联表';
COMMENT ON COLUMN "t_permission_request_cond"."cond_id" IS '条件ID（UUID）';
COMMENT ON COLUMN "t_permission_request_cond"."request_id" IS '关联申请ID';
COMMENT ON COLUMN "t_permission_request_cond"."item_id" IS '关联申请明细ID';
COMMENT ON COLUMN "t_permission_request_cond"."exp_id" IS '权限表达式ID（引用t_permission_exp）';
COMMENT ON COLUMN "t_permission_request_cond"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_permission_request_cond"."create_by" IS '创建人';
COMMENT ON COLUMN "t_permission_request_cond"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_permission_request_cond"."update_by" IS '更新人';
COMMENT ON COLUMN "t_permission_request_cond"."version" IS '版本';
COMMENT ON COLUMN "t_permission_request_cond"."deleted" IS '逻辑删除标记';

-- ----------------------------
-- 表结构：t_permission_auto_approve（自动批准配置表）
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_auto_approve";

CREATE TABLE "t_permission_auto_approve" (
  "id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "permission_id" varchar(32) COLLATE "pg_catalog"."default" NOT NULL,
  "enabled" bool NOT NULL DEFAULT true,
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false,
  PRIMARY KEY ("id")
);

COMMENT ON TABLE "t_permission_auto_approve" IS '自动批准配置表';
COMMENT ON COLUMN "t_permission_auto_approve"."id" IS '配置ID（UUID）';
COMMENT ON COLUMN "t_permission_auto_approve"."permission_id" IS '权限ID';
COMMENT ON COLUMN "t_permission_auto_approve"."enabled" IS '是否启用自动批准';
COMMENT ON COLUMN "t_permission_auto_approve"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_permission_auto_approve"."create_by" IS '创建人';
COMMENT ON COLUMN "t_permission_auto_approve"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_permission_auto_approve"."update_by" IS '更新人';
COMMENT ON COLUMN "t_permission_auto_approve"."version" IS '版本';
COMMENT ON COLUMN "t_permission_auto_approve"."deleted" IS '逻辑删除标记';

-- ----------------------------
-- 索引：t_permission_request
-- ----------------------------
CREATE INDEX "idx_t_permission_request_user" ON "t_permission_request" ("user_id");
CREATE INDEX "idx_t_permission_request_status" ON "t_permission_request" ("status");

-- ----------------------------
-- 索引：t_permission_request_item
-- ----------------------------
CREATE INDEX "idx_t_permission_request_item_request" ON "t_permission_request_item" ("request_id");
CREATE INDEX "idx_t_permission_request_item_permission" ON "t_permission_request_item" ("permission_id");

-- ----------------------------
-- 索引：t_permission_request_cond
-- ----------------------------
CREATE INDEX "idx_t_permission_request_cond_request" ON "t_permission_request_cond" ("request_id");
CREATE INDEX "idx_t_permission_request_cond_item" ON "t_permission_request_cond" ("item_id");

-- ----------------------------
-- 索引：t_permission_auto_approve
-- ----------------------------
CREATE INDEX "idx_t_permission_auto_approve_permission" ON "t_permission_auto_approve" ("permission_id");

-- ----------------------------
-- 外键约束
-- ----------------------------
ALTER TABLE "t_permission_request_item" ADD CONSTRAINT "fk_t_permission_request_item_request" FOREIGN KEY ("request_id") REFERENCES "t_permission_request" ("request_id");
ALTER TABLE "t_permission_request_cond" ADD CONSTRAINT "fk_t_permission_request_cond_request" FOREIGN KEY ("request_id") REFERENCES "t_permission_request" ("request_id");
ALTER TABLE "t_permission_request_cond" ADD CONSTRAINT "fk_t_permission_request_cond_item" FOREIGN KEY ("item_id") REFERENCES "t_permission_request_item" ("item_id");
