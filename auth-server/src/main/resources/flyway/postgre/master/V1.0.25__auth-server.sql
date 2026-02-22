/**
 * 变更：
 *      1. 创建异步任务表 t_async_task
 *
 */

-- 异步任务表
CREATE TABLE "t_async_task" (
    -- 主键
    "task_id" VARCHAR(36) NOT NULL PRIMARY KEY,
    -- 任务类型（如：USER_IMPORT, USER_EXPORT）
    "task_type" VARCHAR(50) NOT NULL,
    -- 任务名称
    "task_name" VARCHAR(200) NOT NULL,
    -- 任务状态：PENDING-等待中, RUNNING-执行中, SUCCESS-成功, FAILED-失败, CANCELLED-已取消
    "status" VARCHAR(20) NOT NULL DEFAULT 'PENDING',
    -- 任务参数（JSON格式）
    "task_params" TEXT,
    -- 任务结果（JSON格式）
    "task_result" TEXT,
    -- 结果文件路径
    "result_file_path" VARCHAR(500),
    -- 结果文件名称
    "result_file_name" VARCHAR(200),
    -- 错误信息
    "error_message" TEXT,
    -- 进度百分比（0-100）
    "progress" INT DEFAULT 0,
    -- 创建者ID
    "create_by" VARCHAR(36),
    -- 创建时间
    "create_time" TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    -- 更新者ID
    "update_by" VARCHAR(36),
    -- 更新时间
    "update_time" TIMESTAMP,
    -- 版本号（用于乐观锁）
    "version" INT DEFAULT 1 NOT NULL,
    -- 逻辑删除标记
    "deleted" BOOL DEFAULT false NOT NULL,
    -- 任务开始时间
    "start_time" TIMESTAMP,
    -- 任务结束时间
    "end_time" TIMESTAMP,
    -- 任务执行耗时（毫秒）
    "duration" BIGINT
);

-- 添加任务ID索引
CREATE INDEX "idx_async_task_id" ON "t_async_task" ("task_id");
-- 添加任务类型索引
CREATE INDEX "idx_async_task_type" ON "t_async_task" ("task_type");
-- 添加任务状态索引
CREATE INDEX "idx_async_task_status" ON "t_async_task" ("status");
-- 添加创建者ID索引
CREATE INDEX "idx_async_task_create_by" ON "t_async_task" ("create_by");
-- 添加创建时间索引
CREATE INDEX "idx_async_task_create_time" ON "t_async_task" ("create_time");

-- 添加注释
COMMENT ON TABLE "t_async_task" IS '异步任务表';
COMMENT ON COLUMN "t_async_task"."task_id" IS '任务ID';
COMMENT ON COLUMN "t_async_task"."task_type" IS '任务类型（如：USER_IMPORT, USER_EXPORT）';
COMMENT ON COLUMN "t_async_task"."task_name" IS '任务名称';
COMMENT ON COLUMN "t_async_task"."status" IS '任务状态：PENDING-等待中, RUNNING-执行中, SUCCESS-成功, FAILED-失败, CANCELLED-已取消';
COMMENT ON COLUMN "t_async_task"."task_params" IS '任务参数（JSON格式）';
COMMENT ON COLUMN "t_async_task"."task_result" IS '任务结果（JSON格式）';
COMMENT ON COLUMN "t_async_task"."result_file_path" IS '结果文件路径';
COMMENT ON COLUMN "t_async_task"."result_file_name" IS '结果文件名称';
COMMENT ON COLUMN "t_async_task"."error_message" IS '错误信息';
COMMENT ON COLUMN "t_async_task"."progress" IS '进度百分比（0-100）';
COMMENT ON COLUMN "t_async_task"."create_by" IS '创建者ID';
COMMENT ON COLUMN "t_async_task"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_async_task"."update_by" IS '更新者ID';
COMMENT ON COLUMN "t_async_task"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_async_task"."version" IS '版本号（用于乐观锁）';
COMMENT ON COLUMN "t_async_task"."deleted" IS '逻辑删除标记';
COMMENT ON COLUMN "t_async_task"."start_time" IS '任务开始时间';
COMMENT ON COLUMN "t_async_task"."end_time" IS '任务结束时间';
COMMENT ON COLUMN "t_async_task"."duration" IS '任务执行耗时（毫秒）';
