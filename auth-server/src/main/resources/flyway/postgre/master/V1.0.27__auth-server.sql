/**
 * 变更：
 *      1. 为租户表 t_tenant 添加生效时间和失效时间字段
 *
 */

-- 为租户表添加生效时间字段
ALTER TABLE "t_tenant" ADD COLUMN "effective_time" timestamp(6);

-- 为租户表添加失效时间字段
ALTER TABLE "t_tenant" ADD COLUMN "expiration_time" timestamp(6);

-- 添加注释
COMMENT ON COLUMN "t_tenant"."effective_time" IS '生效时间';
COMMENT ON COLUMN "t_tenant"."expiration_time" IS '失效时间';
