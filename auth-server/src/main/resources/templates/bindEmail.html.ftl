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
        <div class="tip">${emailCodeLive} 分钟内有效，请尽快填写完成验证。</div>
    </div>
    <p style="border-top: 1px solid #e5e6eb; margin-top: 32px"></p>
    <div class="footer-text">此为系统邮件，请勿回复。</div>
</div>
</body>
</html>
