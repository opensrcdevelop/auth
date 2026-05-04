/**
 * 变更说明：
 *     1. 表【t_chat_message_history】添加 model_provider_id、model 字段
 */

-- ----------------------------
-- 表【t_chat_message_history】添加字段
-- ----------------------------
ALTER TABLE "t_chat_message_history" ADD COLUMN IF NOT EXISTS "model_provider_id" varchar(50);
ALTER TABLE "t_chat_message_history" ADD COLUMN IF NOT EXISTS "model" varchar(255);

COMMENT ON COLUMN "t_chat_message_history"."model_provider_id" IS '模型提供商ID';
COMMENT ON COLUMN "t_chat_message_history"."model" IS '模型';
