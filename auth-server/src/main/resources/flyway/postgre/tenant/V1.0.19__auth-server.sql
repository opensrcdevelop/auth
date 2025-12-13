/**
 * 变更：
 *      1. 表【t_user】添加字段
 *      2. 表【t_permission】添加数据
 *
 */

-- ----------------------------
-- 添加字段
-- ----------------------------
ALTER TABLE "t_user"
    DROP COLUMN IF EXISTS "remember_me_token_secret",
    ADD COLUMN "remember_me_token_secret" varchar(255);

COMMENT ON COLUMN "t_user"."remember_me_token_secret" IS '记住我 token 密钥';


-- ----------------------------
-- 添加数据
-- ----------------------------
DELETE  FROM "t_permission" WHERE "permission_id" = '019a9c8b-9755-7a41-bd6c-1a9340a32472';
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019a9c8b-9755-7a41-bd6c-1a9340a32472', '对话', 'chat', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2025-11-19 22:36:25.302561', 'admin', NULL, NULL, 1, 'f');
