/**
 * 变更：
 *      1. 表【t_permission】添加数据
 *
 */

-- ----------------------------
-- 添加数据
-- ----------------------------
DELETE  FROM "t_permission" WHERE "permission_id" = '019ba77c-2606-795c-a666-0fbada16f1e2';
DELETE  FROM "t_permission" WHERE "permission_id" = '019ba77b-fee3-787e-97ff-6ba1e9b76929';

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019ba77c-2606-795c-a666-0fbada16f1e2', '从 Excel 导入用户数据', 'import', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2026-01-10 18:38:09.927265', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019ba77b-fee3-787e-97ff-6ba1e9b76929', '导出用户数据到 Excel', 'export', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2026-01-10 18:37:59.908757', 'admin', NULL, NULL, 1, 'f');
