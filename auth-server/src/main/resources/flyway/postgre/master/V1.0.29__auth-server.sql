/**
 * 变更：
 *      1. 添加 ChatBI 对话配置到系统设置表
 *
 */

-- 添加 ChatBI 对话配置
INSERT INTO "t_sys_setting" ("key", "value", "description")
VALUES ('chatbi.chat.config', '{"maxSteps": 30, "language": "简体中文", "apiRetryCount": 3}',
        'ChatBI 对话配置：最大思考步数、回答语言、API重试次数');
