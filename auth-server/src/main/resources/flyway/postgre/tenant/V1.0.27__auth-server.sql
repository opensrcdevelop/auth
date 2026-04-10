/**
 * 变更：
 *      1. 表【t_permission】添加删除数据
 *      2. 表【t_model_provider】删除字段
 *
 */

DELETE FROM "t_permission" WHERE "permission_id" IN (
    '019d4ece-4204-7883-80df-55f4810fbd65',
    '019d4ece-2555-75f3-b2af-677305db5bcc'
);

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019d4ece-4204-7883-80df-55f4810fbd65', '更新对话配置', 'updateChatConfig', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2026-04-02 23:27:13.412738', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019d4ece-2555-75f3-b2af-677305db5bcc', '获取对话配置', 'getChatConfig', NULL, '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa', '2026-04-02 23:27:06.072187', 'admin', NULL, NULL, 1, 'f');


ALTER TABLE "t_model_provider" DROP COLUMN "temperature";
ALTER TABLE "t_model_provider" DROP COLUMN "max_tokens";