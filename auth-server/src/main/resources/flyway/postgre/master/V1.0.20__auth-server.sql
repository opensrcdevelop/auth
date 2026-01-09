/**
 * 变更：
 *      1. 表【t_user】添加字段
 *      2. 表【t_permission】添加数据
 *      3. 表【t_mail_template】修改数据
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

-- ----------------------------
-- 修改数据
-- ----------------------------
UPDATE "t_mail_template" SET "template_content" = '<!DOCTYPE html>
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
    <div class="tip-text">以下是你的账号详细信息。请尽快登录，及时修改初始密码。</div>
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
', "subject" = '请查收您的 Auth Server 账号信息。', "description" = '当创建用户时，会发送此电子邮件。' WHERE "template_id" = '8a002bb8-b7ff-486e-adb3-f5b24cd69ca1';
