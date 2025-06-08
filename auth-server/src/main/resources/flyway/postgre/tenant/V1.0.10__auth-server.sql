/**
 * 变更：
 *      1. 表【t_resource】添加/修改数据
 *      2. 表【t_permission】添加数据
 *      3. 表【t_authorize】添加数据
 *      4. 表【t_sys_setting】添加数据
 *
 */

-- ----------------------------
-- Table data for t_resource
-- ----------------------------
UPDATE "t_resource" SET "api_identifier" = '/api/v1/setting/message' WHERE "resource_id" = '3c290ba3-b34a-4a42-8df9-bc8939d999e8';
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('bb4a7fe7-719d-49fa-bd73-597df6bc5ac3', 'JWT 设置', 'jwtSetting', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/setting/jwt', NULL, '2025-04-11 22:14:21.442469', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Table data for t_permission
-- ----------------------------
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('c0e68b77-013c-4d32-af75-d2a7438c949a', '获取 JWT 密钥信息', 'getSecretInfo', NULL, 'bb4a7fe7-719d-49fa-bd73-597df6bc5ac3', '2025-04-11 22:15:00.439762', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('a93d172a-3b1b-4c86-8b18-6226f8e96c24', '获取 JWT 密钥轮换配置', 'getRotationConfig', NULL, 'bb4a7fe7-719d-49fa-bd73-597df6bc5ac3', '2025-04-11 22:15:41.804657', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('4176b773-6661-41d8-a03f-1e8081140a24', '更新 JWT 密钥轮换设置', 'updateRotationConfig', NULL, 'bb4a7fe7-719d-49fa-bd73-597df6bc5ac3', '2025-04-11 22:16:42.42397', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5a1c2ac0-91ec-4cba-a4f6-0e64f0cfb5fc', '轮换 JWT 密钥', 'rotate', NULL, 'bb4a7fe7-719d-49fa-bd73-597df6bc5ac3', '2025-04-11 22:17:07.100522', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('7c639292-49c8-489d-9adc-1b739579fc08', '所有权限', 'all', NULL, 'bb4a7fe7-719d-49fa-bd73-597df6bc5ac3', '2025-04-11 22:18:49.207262', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Table data for t_authorize
-- ----------------------------
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '7c639292-49c8-489d-9adc-1b739579fc08', '2236fd93-34f1-4df5-9574-7fef360219e9', '2025-04-11 23:33:02.724135', 0);

-- ----------------------------
-- Table data for t_sys_setting
-- ----------------------------
INSERT INTO "t_sys_setting" ("key", "value", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('jwt.secret.rotation.config', '{"rotationPeriod":3,"rotationPeriodUnit":"MONTH"}', NULL, '2025-04-10 22:26:42.8537', 'admin', NULL, NULL, 1, 'f');