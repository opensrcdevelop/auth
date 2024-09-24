<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title></title>
    <style type="text/css">
        @import url(https://fonts.googleapis.com/css?family=Ubuntu:300,400,500,700);

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
            width: 540px;
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
        }

        .detail-container .detail-item .left {
            padding-left: 12px;
            width: 145px;
            display: inline-block;
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
            <span class="left">用户名</span>
            <span class="right">${username}</span>
        </div>
        <div class="detail-item" style="margin-top: 1px">
            <span class="left">初始密码</span>
            <span class="right">${password}</span>
        </div>
        <div
                class="detail-item"
                style="margin-top: 1px; border-radius: 0 0 8px 8px"
        >
            <span class="left">登录地址</span>
            <span class="right">
            <a
                    href="${loginUrl}"
                    style="text-decoration: none; color: #215ae5"
            >${loginUrl}</a
            >
          </span>
        </div>
    </div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 32px"></p>
    <div class="footer-text">此为系统邮件，请勿回复。</div>
</div>
</body>
</html>
