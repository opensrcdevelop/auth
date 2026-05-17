/**
 * 变更说明：
 *     1. 表【t_permission】添加删除数据
 */

DELETE FROM "t_permission" WHERE "permission_id" IN (
    '019e34b6-128d-7e56-9220-e3a33c309639',
    '019e34b5-e3a0-729b-bf13-9e0a091ffbd0',
    '019e34b5-99c6-7604-917e-22c5ac0f5364'
);

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "allow_apply", "auto_approve") VALUES ('019e34b6-128d-7e56-9220-e3a33c309639', '删除 CSV 文件', 'deleteCsv', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2026-05-17 14:53:28.078233', 'admin', NULL, NULL, 1, 'f', 't', 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "allow_apply", "auto_approve") VALUES ('019e34b5-e3a0-729b-bf13-9e0a091ffbd0', '获取 CSV 文件列表', 'getCsvList', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2026-05-17 14:53:16.064639', 'admin', NULL, NULL, 1, 'f', 't', 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "allow_apply", "auto_approve") VALUES ('019e34b5-99c6-7604-917e-22c5ac0f5364', '上传 CSV 文件', 'uploadCsv', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2026-05-17 14:52:57.159425', 'admin', NULL, NULL, 1, 'f', 't', 'f');
