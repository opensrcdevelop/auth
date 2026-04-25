/**
 * 变更：
 *      1. 表【t_resource、t_permission、t_authorize】添加删除数据
 *
 */

DELETE FROM "t_resource" WHERE "resource_id" = '019cfc50-fa16-721c-bea5-e03647a7029f';
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc50-fa16-721c-bea5-e03647a7029f', 'ChatBI 问数-示例 SQL', 'chatBISampleSql', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/chatbi/sampleSql', NULL, '2026-03-17 23:01:31.287073', 'admin', NULL, NULL, 1, 'f');

DELETE FROM "t_permission" WHERE "permission_id" IN (
    '019d00df-1720-7d60-bfbb-6003fd13088c',
    '019cfc54-157b-78bc-a421-bb375363d42e',
    '019cfc53-b449-764d-a796-7a8f96ecf670',
    '019cfc53-52f7-73a1-9df1-775ae4410076',
    '019cfc52-d641-7208-a66b-6a173ae9e906',
    '019cfc52-7daf-78d9-8e1f-788315caf3a8',
    '019cfc52-5736-7020-90d0-e8600f580d00',
    '019cfc51-abdf-7051-9173-c35f97b4c3e7'
);
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019d00df-1720-7d60-bfbb-6003fd13088c', '获取示例 SQL 列表', 'list', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-18 20:15:13.697105', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc54-157b-78bc-a421-bb375363d42e', '更新示例 SQL 嵌入配置', 'updateEmbeddingConfig', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-17 23:04:54.907851', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc53-b449-764d-a796-7a8f96ecf670', '获取示例 SQL 嵌入配置', 'getEmbeddingConfig', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-17 23:04:30.025582', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc53-52f7-73a1-9df1-775ae4410076', '重新构建示例 SQL 索引', 'rebuildIndex', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-17 23:04:05.111423', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc52-d641-7208-a66b-6a173ae9e906', '从 Likes 同步示例 SQL', 'syncFromLikes', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-17 23:03:33.185588', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc52-7daf-78d9-8e1f-788315caf3a8', '删除示例 SQL', 'delete', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-17 23:03:10.511984', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc52-5736-7020-90d0-e8600f580d00', '添加示例 SQL', 'create', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-17 23:03:00.662343', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019cfc51-abdf-7051-9173-c35f97b4c3e7', '所有权限', 'all', NULL, '019cfc50-fa16-721c-bea5-e03647a7029f', '2026-03-17 23:02:16.800232', 'admin', NULL, NULL, 1, 'f');

DELETE FROM "t_authorize" WHERE "authorize_id" = '019d00e2-6ca2-7726-b0cd-1af7556a4a83';
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '019cfc51-abdf-7051-9173-c35f97b4c3e7', '019d00e2-6ca2-7726-b0cd-1af7556a4a83', '2026-03-18 20:18:52.193989', 0);
