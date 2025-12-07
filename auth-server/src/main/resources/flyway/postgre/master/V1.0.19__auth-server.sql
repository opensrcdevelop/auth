/**
 * 变更：
 *      1. 修改表【t_resource】
 *      2. 表【t_permission】添加删除数据
 *      3. 表【t_user_group】添加字段
 *
 */

-- ----------------------------
-- 更新资源标识
-- ----------------------------
UPDATE "t_resource" SET "resource_name" = 'ChatBI 问数' WHERE "resource_id" = '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa';

-- ----------------------------
-- 添加权限
-- ----------------------------
DELETE FROM "t_permission" WHERE "permission_id" = '019a9c8c-37da-747e-aa4b-bf125db5702d';
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019a9c8c-37da-747e-aa4b-bf125db5702d', '获取回答的 SQL', 'getAnsweredSql', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-11-19 22:37:06.3946', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- 添加字段
-- ----------------------------
ALTER TABLE "t_user_group"
    DROP COLUMN IF EXISTS "user_group_type",
    DROP COLUMN IF EXISTS "dynamic_conditions",
    ADD COLUMN "user_group_type" varchar(50),
    ADD COLUMN "dynamic_conditions" text;

COMMENT ON COLUMN "t_user_group"."user_group_type" IS '用户组类型。STATIC：静态用户组、DYNAMIC：动态用户组';
COMMENT ON COLUMN "t_user_group"."dynamic_conditions" IS '动态用户组条件';

-- ----------------------------
-- 初始化用户组类型
-- ----------------------------
UPDATE "t_user_group" SET "user_group_type" = 'STATIC' WHERE "user_group_type" IS NULL;