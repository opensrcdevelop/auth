/**
 * 变更：
 *      1. 表【t_user】添加字段
 *
 */

-- ----------------------------
-- 添加字段
-- ----------------------------
ALTER TABLE "t_user"
    DROP COLUMN IF EXISTS "remember_me_token_secret",
    ADD COLUMN "remember_me_token_secret" varchar(255);

COMMENT ON COLUMN "t_user"."remember_me_token_secret" IS '记住我 token 密钥';
