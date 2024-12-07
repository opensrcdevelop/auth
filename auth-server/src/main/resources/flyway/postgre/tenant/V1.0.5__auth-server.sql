/**
 * 变更：
 *      1. 表【t_user】删除字段【last_login_time、last_login_ip、last_login_device_type、last_login_device_os】
 *      2. 添加表【t_login_log】
 *      3. 表【t_authorization】添加字典【login_id】
 *      4. 表【t_permission】添加数据
 *
 */

ALTER TABLE "t_user" 
  DROP COLUMN IF EXISTS "last_login_time",
  DROP COLUMN IF EXISTS "last_login_ip",
  DROP COLUMN IF EXISTS "last_login_device_type",
  DROP COLUMN IF EXISTS "last_login_device_os";

-- ----------------------------
-- Sequence structure for t_login_log_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_login_log_id_seq";
CREATE SEQUENCE "t_login_log_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

-- ----------------------------
-- Table structure for t_login_log
-- ----------------------------
DROP TABLE IF EXISTS "t_login_log";
CREATE TABLE "t_login_log" (
  "id" int8 NOT NULL DEFAULT nextval('t_login_log_id_seq'::regclass),
  "login_id" varchar(50) COLLATE "pg_catalog"."default",
  "user_id" varchar(50) COLLATE "pg_catalog"."default",
  "session_id" varchar(50) COLLATE "pg_catalog"."default",
  "client_id" varchar(50) COLLATE "pg_catalog"."default",
  "login_ip" varchar(255) COLLATE "pg_catalog"."default",
  "device_type" varchar(255) COLLATE "pg_catalog"."default",
  "device_os" varchar(255) COLLATE "pg_catalog"."default",
  "browser_type" varchar(255) COLLATE "pg_catalog"."default",
  "login_time" timestamp(6)
)
;
COMMENT ON COLUMN "t_login_log"."id" IS '主键';
COMMENT ON COLUMN "t_login_log"."login_id" IS '登录ID';
COMMENT ON COLUMN "t_login_log"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_login_log"."session_id" IS '会话ID';
COMMENT ON COLUMN "t_login_log"."client_id" IS '客户端ID';
COMMENT ON COLUMN "t_login_log"."login_ip" IS '登录IP';
COMMENT ON COLUMN "t_login_log"."device_type" IS '设备类型';
COMMENT ON COLUMN "t_login_log"."device_os" IS '设备OS';
COMMENT ON COLUMN "t_login_log"."browser_type" IS '浏览器类型';
COMMENT ON COLUMN "t_login_log"."login_time" IS '登录时间';

-- ----------------------------
-- Primary Key structure for table t_login_log
-- ----------------------------
ALTER TABLE "t_login_log" ADD CONSTRAINT "t_login_log_pkey" PRIMARY KEY ("id");

ALTER TABLE "t_authorization" 
  DROP COLUMN IF EXISTS "login_id",
  ADD COLUMN "login_id" varchar(50);

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('49cd93fb-bf74-4720-a32b-5fab685a8306', '获取用户登录日志', 'loginLogs', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-12-07 14:27:49.092342', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('55582719-507c-4a5f-a22d-ea0f9756aaea', '清除登录 ID 关联的 Token', 'clearTokensByLoginId', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-12-07 14:29:09.81827', 'admin', NULL, NULL, 1, 'f');
