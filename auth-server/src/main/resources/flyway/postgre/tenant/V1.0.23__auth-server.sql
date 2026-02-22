/**
 * 变更：
 *      1. 表【t_permission】添加修改数据
 *
 */

UPDATE "t_permission" SET "permission_code" = 'rebindTotpDevice', "permission_name" = '重新绑定 TOTP 设备' WHERE "permission_id" = '649fad42-fa6a-4b1f-9cc0-421c24ee8619';

DELETE  FROM "t_permission" WHERE "permission_id" = '019c47d6-506e-738d-9256-03b70ba75533';
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019c47d6-506e-738d-9256-03b70ba75533', '清除注册的 Passkey 凭证', 'clearPasskeyCredentials', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2026-02-10 21:55:53.586544', 'admin', NULL, NULL, 1, 'f');
