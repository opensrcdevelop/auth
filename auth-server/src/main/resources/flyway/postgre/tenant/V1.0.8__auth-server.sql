/**
 * 变更：
 *      1. 添加表【t_mail_template】
 *      2. 添加表【t_sys_setting】
 *      3. 表【t_resource】添加数据
 *      4. 表【t_permission】添加数据
 *      5. 表【t_authorize】添加字段【priority】
 *      6. 表【t_authorize】添加数据
 *
 */

DROP SEQUENCE IF EXISTS "t_mail_template_id_seq";
CREATE SEQUENCE "t_mail_template_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

DROP TABLE IF EXISTS "t_mail_template";
CREATE TABLE "t_mail_template" (
  "id" int8 NOT NULL DEFAULT nextval('t_mail_template_id_seq'::regclass),
  "template_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "template_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "template_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "template_content" text COLLATE "pg_catalog"."default" NOT NULL,
  "template_parameters" text COLLATE "pg_catalog"."default",
  "subject" text COLLATE "pg_catalog"."default" NOT NULL,
  "sender" text COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
);

COMMENT ON COLUMN "t_mail_template"."id" IS '主键';
COMMENT ON COLUMN "t_mail_template"."template_id" IS '模版ID';
COMMENT ON COLUMN "t_mail_template"."template_code" IS '模版标识';
COMMENT ON COLUMN "t_mail_template"."template_name" IS '模版名称';
COMMENT ON COLUMN "t_mail_template"."template_content" IS '模版内容';
COMMENT ON COLUMN "t_mail_template"."template_parameters" IS '模版参数';
COMMENT ON COLUMN "t_mail_template"."subject" IS '主题';
COMMENT ON COLUMN "t_mail_template"."sender" IS '发送人';
COMMENT ON COLUMN "t_mail_template"."description" IS '描述';

ALTER TABLE "t_mail_template" ADD CONSTRAINT "t_mail_template_pkey" PRIMARY KEY ("id");

INSERT INTO "t_mail_template" ("template_id", "template_code", "template_name", "template_content", "template_parameters", "subject", "sender", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ed59dc12-935f-4c58-a8fc-d18f1884a4c2', 'bind_email', '绑定邮箱', '<!DOCTYPE html>
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
            text-align: center;
        }

        .container .tip-text {
            text-align: center;
            color: #000000;
            font-size: 14px;
            font-weight: 400;
            margin-top: 16px;
        }

        .code-container {
            width: 100%;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
        }

        .code-container .text {
            text-align: center;
            background-color: #f7f8fa;
            font-size: 40px;
            line-height: 40px;
            width: 210px;
            color: #1d2129;
            box-sizing: content-box;
            letter-spacing: 10px;
            padding: 13px 8px 11px 23px;
            margin-top: 10px;
            border-radius: 4px 4px 0 0;
        }

        .code-container .tip {
            text-align: center;
            background-color: #f7f8fa;
            color: #86909c;
            font-size: 12px;
            padding: 10px 0;
            width: 240px;
            margin-top: 1px;
            border-radius: 0 0 4px 4px;
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
    <div class="tip-text">你正在绑定邮箱，验证码为</div>
    <div class="code-container">
        <div class="text">${code}</div>
        <div class="tip">${code_live} 分钟内有效，请尽快填写完成验证。</div>
    </div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 32px"></p>
    <div class="footer-text">此为系统邮件，请勿回复。</div>
</div>
</body>
</html>
', '{
    "username": "收件人用户名",
    "code": "验证码",
    "code_live": "验证码有效时间",
    "sender": "发件人"
}', '您正在绑定邮箱，请查收验证码', 'Auth Server<${sender}>', '当用户绑定邮箱时，会发送此电子邮件。', '2025-03-22 14:50:18', 'admin', NULL, NULL, 1, 'f');

INSERT INTO "t_mail_template" ("template_id", "template_code", "template_name", "template_content", "template_parameters", "subject", "sender", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('1a326ffe-08bd-410a-aca0-9277cce2c44d', 'mail_verify', '邮箱验证', '<!DOCTYPE html>
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
            text-align: center;
        }

        .container .tip-text {
            text-align: center;
            color: #000000;
            font-size: 14px;
            font-weight: 400;
            margin-top: 16px;
        }

        .code-container {
            width: 100%;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
        }

        .code-container .text {
            text-align: center;
            background-color: #f7f8fa;
            font-size: 40px;
            line-height: 40px;
            width: 210px;
            color: #1d2129;
            box-sizing: content-box;
            letter-spacing: 10px;
            padding: 13px 8px 11px 23px;
            margin-top: 10px;
            border-radius: 4px 4px 0 0;
        }

        .code-container .tip {
            text-align: center;
            background-color: #f7f8fa;
            color: #86909c;
            font-size: 12px;
            padding: 10px 0;
            width: 240px;
            margin-top: 1px;
            border-radius: 0 0 4px 4px;
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
    <div class="tip-text">你的账号正在进行邮箱验证，验证码为</div>
    <div class="code-container">
        <div class="text">${code}</div>
        <div class="tip">${code_live} 分钟内有效，请尽快填写完成验证。</div>
    </div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 32px"></p>
    <div class="footer-text">此为系统邮件，请勿回复。</div>
</div>
</body>
</html>', '{
    "username": "收件人用户名",
    "code": "验证码",
    "code_live": "验证码有效时间",
    "sender": "发件人"
}', '您正在进行邮箱验证，请查收验证码。', 'Auth Server<${sender}>', '当用户进行邮箱验证时，会发送此电子邮件。', '2025-03-22 22:18:12', 'admin', NULL, NULL, 1, 'f');

INSERT INTO "t_mail_template" ("template_id", "template_code", "template_name", "template_content", "template_parameters", "subject", "sender", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('8a5fd063-e8ea-478c-98da-0e8eed303067', 'reset_password', '重置密码', '<!DOCTYPE html>
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

        .detail-container {
            width: 100%;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            margin-top: 12px;
        }

        .detail-container .detail-item {
            width: 100%;
            background: #f7f8fa;
            padding: 12px 0;
            display: flex;
            align-items: center;
        }

        .detail-container .detail-item .left {
            padding-left: 12px;
            width: 145px;
        }

        detail-container .detail-item .right {
            padding-right: 12px;
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
    <div class="tip-text">管理员已将你的密码重置，请尽快登录并修改密码！</div>
    <div class="detail-container">
        <div class="detail-item" style="border-radius: 8px 8px 0 0">
            <div class="left">用户名</div>
            <div class="right">${username}</div>
        </div>
        <div class="detail-item" style="margin-top: 1px">
            <div class="left">重置后的密码</div>
            <div class="right">${password}</div>
        </div>
        <div
                class="detail-item"
                style="margin-top: 1px; border-radius: 0 0 8px 8px"
        >
            <div class="left">登录地址</div>
            <div class="right">
            <a
                    href="${login_url}"
                    style="text-decoration: none; color: #215ae5"
            >${login_url}</a>
          </div>
        </div>
    </div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 32px"></p>
    <div class="footer-text">此为系统邮件，请勿回复。</div>
</div>
</body>
</html>', '{
    "username": "收件人用户名",
    "password": "重置后的密码",
    "login_url": "登录地址",
    "sender": "发件人"
}', '管理已将您的密码重置，请查收账号信息。', 'Auth Server<${sender}>', '当管理员重置用户密码时，会发送此电子邮件。', '2025-03-22 22:30:31', 'admin', NULL, NULL, 1, 'f');

INSERT INTO "t_mail_template" ("template_id", "template_code", "template_name", "template_content", "template_parameters", "subject", "sender", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('8a002bb8-b7ff-486e-adb3-f5b24cd69ca1', 'create_user', '创建用户', '<!DOCTYPE html>
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

        .detail-container {
            width: 100%;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            margin-top: 12px;
        }

        .detail-container .detail-item {
            width: 100%;
            background: #f7f8fa;
            padding: 12px 0;
            display: flex;
            align-items: center;
        }

        .detail-container .detail-item .left {
            padding-left: 12px;
            width: 145px;
        }

        .detail-container .detail-item .right {
            padding-right: 12px;
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
    <div class="tip-text">管理员为你创建了账号，以下是详细信息</div>
    <div class="detail-container">
        <div class="detail-item" style="border-radius: 8px 8px 0 0">
            <div class="left">用户名</div>
            <div class="right">${username}</div>
        </div>
        <div class="detail-item" style="margin-top: 1px">
            <div class="left">初始密码</div>
            <div class="right">${password}</div>
        </div>
        <div
                class="detail-item"
                style="margin-top: 1px; border-radius: 0 0 8px 8px"
        >
            <div class="left">登录地址</div>
            <div class="right">
            <a
                    href="${login_url}"
                    style="text-decoration: none; color: #215ae5"
            >${login_url}</a
            >
          </div>
        </div>
    </div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 32px"></p>
    <div class="footer-text">此为系统邮件，请勿回复。</div>
</div>
</body>
</html>
', '{
    "username": "收件人用户名",
    "password": "初始密码",
    "login_url": "登录地址",
    "sender": "发件人"
}', '管理员为您创建了账号，请查收账号信息。', 'Auth Server<${sender}>', '当管理员创建用户时，会发送此电子邮件。', '2025-03-22 22:27:58', 'admin', NULL, NULL, 1, 'f');

DROP TABLE IF EXISTS "t_sys_setting";
CREATE TABLE "public"."t_sys_setting" (
  "key" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "value" text COLLATE "pg_catalog"."default",
  "description" varchar(500) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
);

COMMENT ON COLUMN "t_sys_setting"."key" IS '系统配置键';
COMMENT ON COLUMN "t_sys_setting"."value" IS '系统配置值';
COMMENT ON COLUMN "t_sys_setting"."description" IS '系统配置描述';

ALTER TABLE "t_sys_setting" ADD CONSTRAINT "t_sys_setting_pkey" PRIMARY KEY ("key");

INSERT INTO "t_sys_setting" ("key", "value", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('mail.message.config', '{"codeLive":5}', NULL, '2025-03-23 14:13:41.528672', 'admin', NULL, NULL, 1, 'f');

INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('3c290ba3-b34a-4a42-8df9-bc8939d999e8', '系统设置', 'setting', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/setting', NULL, '2025-03-23 15:51:21.361385', 'admin', NULL, NULL, 1, 'f');

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('d2dc166d-814f-498c-8cab-073c71ad9498', '更新授权优先级', 'updateAuthorizePriority', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2025-03-19 22:51:02.885466', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('26f49386-70c6-401b-a94f-6458a66cab20', '获取邮件模版列表', 'mailTemplateList', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:53:27.509929', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('fc5442e9-4f59-4fe6-91ee-b3f951489359', '获取邮件模版详情', 'mailTemplateDetail', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:53:52.950344', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('6f323e5a-bac8-4049-861d-16748f4f53fc', '更新邮件模版', 'updateMailTemplate', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:54:11.458672', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b1144b56-00e8-4897-94db-912465ea4010', '获取邮件服务配置', 'getMailServiceConfig', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:54:42.389169', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('80605e65-e629-4c7a-9707-eef17ad386ae', '保存邮件服务配置', 'saveMailServiceConfig', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:55:05.493597', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('6d26bfea-f35b-4493-838e-516235d4e2e5', '获取邮件消息配置', 'getMailMessageConfig', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:55:24.384089', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5b21ca18-8b99-40fa-8fea-66f985b56051', '保存邮件消息配置', 'saveMailMessageConfig', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:55:50.403747', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('58ffa319-736c-44f8-8056-3e655940156e', '所有权限', 'all', NULL, '3c290ba3-b34a-4a42-8df9-bc8939d999e8', '2025-03-23 15:56:29.456991', 'admin', NULL, NULL, 1, 'f');

ALTER TABLE "t_authorize" 
  DROP COLUMN IF EXISTS "priority",
  ADD COLUMN "priority" int4 NOT NULL DEFAULT 0;

COMMENT ON COLUMN "t_authorize"."priority" IS '优先级';

INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '58ffa319-736c-44f8-8056-3e655940156e', '501725a2-f6dc-424f-b129-e83eb8f57be4', '2025-03-23 16:05:45.020497', 0);
