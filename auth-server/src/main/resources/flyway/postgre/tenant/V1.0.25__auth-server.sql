/**
 * 变更：
 *      1. 为异步任务表 t_async_task 添加 user_id 字段，用于区分任务创建者
 *
 */

-- 为异步任务表添加 user_id 字段
ALTER TABLE "t_async_task" ADD COLUMN "user_id" VARCHAR(36);

-- 添加 user_id 索引
CREATE INDEX "idx_async_task_user_id" ON "t_async_task" ("user_id");

-- 添加注释
COMMENT ON COLUMN "t_async_task"."user_id" IS '用户ID（用于区分任务创建者）';
