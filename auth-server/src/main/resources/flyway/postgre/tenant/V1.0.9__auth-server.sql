/**
 * 变更：
 *      1. 添加表【t_password_policy】
 *      2. 添加表【t_password_policy_mapping】
 *      3. 添加表【t_update_password_remind_log】
 *      3. 表【t_resource】添加/修改数据
 *      4. 表【t_permission】添加数据
 *      6. 表【t_authorize】添加数据
 *      7. 表【t_mail_template】添加数据
 *      8. 表【t_user】添加字段
 *
 */

-- ----------------------------
-- Table structure for t_password_policy
-- ----------------------------
DROP TABLE IF EXISTS "t_password_policy";
DROP SEQUENCE IF EXISTS "t_password_policy_id_seq";
CREATE SEQUENCE "t_password_policy_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

CREATE TABLE "t_password_policy" (
  "id" int8 NOT NULL DEFAULT nextval('t_password_policy_id_seq'::regclass),
  "policy_id" varchar(50) NOT NULL,
  "policy_name" varchar(255) NOT NULL,
  "description" varchar(500),
  "password_strength" int2 NOT NULL,
  "custom_strength_config" text,
  "enabled" bool NOT NULL,
  "priority" int4 NOT NULL,
  "enable_password_detection" bool,
  "enable_force_change_password" bool,
  "forced_cycle" int4,
  "forced_cycle_unit" varchar(5),
  "remind_cycle" int4,
  "remind_cycle_unit" varchar(5),
  "create_time" timestamp,
  "create_by" varchar(255),
  "update_time" timestamp,
  "update_by" varchar(255),
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
);

COMMENT ON COLUMN "t_password_policy"."id" IS '主键';

COMMENT ON COLUMN "t_password_policy"."policy_id" IS '策略ID';

COMMENT ON COLUMN "t_password_policy"."policy_name" IS '策略名称';

COMMENT ON COLUMN "t_password_policy"."description" IS '策略描述';

COMMENT ON COLUMN "t_password_policy"."password_strength" IS '密码强度';

COMMENT ON COLUMN "t_password_policy"."custom_strength_config" IS '自定义密码强度配置';

COMMENT ON COLUMN "t_password_policy"."enabled" IS '是否启用';

COMMENT ON COLUMN "t_password_policy"."priority" IS '优先级';

COMMENT ON COLUMN "t_password_policy"."enable_password_detection" IS '是否开启用户登录密码强度检查';

COMMENT ON COLUMN "t_password_policy"."enable_force_change_password" IS '是否开启强制修改密码';

COMMENT ON COLUMN "t_password_policy"."forced_cycle" IS '强制修改密码周期';

COMMENT ON COLUMN "t_password_policy"."forced_cycle_unit" IS '强制修改密码周期单位';

COMMENT ON COLUMN "t_password_policy"."remind_cycle" IS '密码到期提醒周期';

COMMENT ON COLUMN "t_password_policy"."remind_cycle_unit" IS '密码到期提醒周期单位';

ALTER TABLE "t_password_policy" ADD CONSTRAINT "t_password_policy_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table data for t_password_policy
-- ----------------------------
INSERT INTO "t_password_policy" ("policy_id", "policy_name", "description", "password_strength", "custom_strength_config", "enabled", "priority", "enable_password_detection", "enable_force_change_password", "forced_cycle", "forced_cycle_unit", "remind_cycle", "remind_cycle_unit", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5f388f5a-7060-43b3-93c2-9276bb335cff', '默认策略', NULL, 0, NULL, 't', 0, 'f', 'f', NULL, NULL, NULL, NULL, '2025-03-30 19:42:46.482062', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Table structure for t_password_policy_mapping
-- ----------------------------
DROP TABLE IF EXISTS "t_password_policy_mapping";
CREATE TABLE "t_password_policy_mapping" (
  "policy_id" varchar(50),
  "user_id" varchar(50),
  "user_group_id" varchar(50)
);

-- ----------------------------
-- Table structure for t_update_password_remind_log
-- ----------------------------
DROP TABLE IF EXISTS "t_update_password_remind_log";
DROP SEQUENCE IF EXISTS "t_update_password_remind_log_id_seq";
CREATE SEQUENCE "t_update_password_remind_log_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

CREATE TABLE "t_update_password_remind_log" (
  "id" int8 NOT NULL DEFAULT nextval('t_update_password_remind_log_id_seq'::regclass),
  "user_id" varchar(50) NOT NULL,
  "policy_id" varchar(50) NOT NULL,
  "remind_method" VARCHAR(15) NOT NULL,
  "remind_time" timestamp NOT NULL,
  "success" bool NOT NULL
);

COMMENT ON COLUMN "t_update_password_remind_log"."id" IS '主键';

COMMENT ON COLUMN "t_update_password_remind_log"."user_id" IS '用户ID';

COMMENT ON COLUMN "t_update_password_remind_log"."policy_id" IS '策略ID';

COMMENT ON COLUMN "t_update_password_remind_log"."remind_method" IS '提醒方式';

COMMENT ON COLUMN "t_update_password_remind_log"."remind_time" IS '提醒时间';

COMMENT ON COLUMN "t_update_password_remind_log"."success" IS '是否成功';

ALTER TABLE "t_update_password_remind_log" ADD CONSTRAINT "t_update_password_remind_log_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table data for t_resource
-- ----------------------------
UPDATE "t_resource" SET "resource_name" = '消息设置', "resource_code" = 'messageSetting' WHERE "resource_id" = '3c290ba3-b34a-4a42-8df9-bc8939d999e8';
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('2d2225a8-5143-4465-ab76-06b101abff8f', '密码策略', 'passwordPolicy', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/setting/passwordPolicy', NULL, '2025-04-06 21:42:31.128355', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Table data for t_permission
-- ----------------------------
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('2906eb93-87a1-44a4-a242-2f3065dfa868', '创建密码策略', 'create', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:43:04.425613', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('14c24e20-731f-4d2b-abc0-4ea73d32f515', '获取密码策略列表', 'list', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:43:19.564801', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('e2533b8b-2869-4c8b-b1e8-46f5b8f769dd', '获取密码策略详情', 'detail', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:43:37.723789', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ef877d20-6688-4243-a023-5da1f1927bc8', '更新密码策略优先级', 'updatePriority', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:43:55.647779', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5d3f755d-3373-4d6c-a0de-8102b1af9d8c', '更新密码策略', 'update', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:44:10.68933', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('8eaaf985-dcf2-4f36-93e4-64cd03192744', '删除密码策略', 'delete', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:44:22.579909', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('90a1b0d0-6870-4480-870b-a9160525125b', '检查密码强度（直接使用密码策略）', 'checkStrength', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:44:36.58162', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('04f6aec7-fdbf-4b4a-a3e6-e262db0eacf7', '所有权限', 'all', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:45:32.476013', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f9727481-42c4-4d71-9c42-ad2faadb1552', '获取修改密码提醒记录列表', 'listRemindLog', NULL, '2d2225a8-5143-4465-ab76-06b101abff8f', '2025-04-06 21:45:05.436528', 'admin', NULL, NULL, 1, 'f');


-- ----------------------------
-- Table data for t_authorize
-- ----------------------------
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '04f6aec7-fdbf-4b4a-a3e6-e262db0eacf7', '1e066f52-baf0-4d8b-a922-0dd227e08606', '2025-04-06 21:58:29.568079', 0);

-- ----------------------------
-- Table data for t_mail_template
-- ----------------------------
INSERT INTO "t_mail_template" ("template_id", "template_code", "template_name", "template_content", "template_parameters", "subject", "sender", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('63ba1aee-c222-4e9e-a97d-e05f1a241d00', 'remind_update_password', '密码到期提醒', '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title></title>
    <style type="text/css">
        * {
            margin: 0;
            padding: 0;
            outline: none !important;
        }

        html,
        body {
            margin: 0;
            padding: 0;
            width: 100%;
            height: 100%;
            background-color: #f7f8fa;
            font-family: PingFang SC, Ubuntu, Helvetica, Arial, sans-serif;
            font-size: 14px;
            display: flex;
            justify-content: center;
            align-items: center;
        }

        .container {
            background-color: #fff;
            min-width: 445px;
            width: 85%;
            border-radius: 8px;
            padding: 14px 32px 14px 32px;
            box-sizing: border-box;
        }

        .container .app {
            line-height: 22px;
            color: #333333;
            font-size: 16px;
            font-weight: 500;
        }

        .container .welcome-text {
            margin-top: 16px;
            font-size: 16px;
            font-weight: 500;
            color: #000000;
        }

        .container .tip-text {
            color: #000000;
            font-size: 14px;
            font-weight: 400;
            margin-top: 16px;
        }

        .footer-text {
            margin-top: 6px;
            font-size: 12px;
            font-weight: 400;
            text-align: left;
            color: #86909c;
        }
    </style>
</head>
<body>
<div class="container">
    <div class="app">Auth Server</div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 16px"></p>
    <div class="welcome-text">你好，${username}</div>
    <div class="tip-text">
        <span>您的账号密码将于 </span>
        <span style="color: #215ae5;">${expire_time}</span>
        <span> 过期，为了不影响您的继续使用，请及时前往个人中心修改密码。</span> <br/>
        <span></span>
    </div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 32px"></p>
    <div class="footer-text">此为系统邮件，请勿回复。</div>
</div>
</body>
</html>', '{
    "username": "收件人用户名",
    "expire_time": "密码过期时间",
    "sender": "发件人"
}', '密码到期提醒。', 'Auth Server<${sender}>', '当用户密码即将过期时，会发送此电子邮件', '2025-04-06 16:56:40', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Table structure for t_user
-- ----------------------------
ALTER TABLE "t_user" 
  DROP COLUMN IF EXISTS "last_update_password_time",
  ADD COLUMN "last_update_password_time" timestamp;

COMMENT ON COLUMN "t_user"."last_update_password_time" IS '最后修改密码时间';