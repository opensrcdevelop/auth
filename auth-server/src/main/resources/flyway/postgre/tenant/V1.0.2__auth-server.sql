/**
 * 变更：
 *      1. 表【t_permission】添加权限【获取用户权限、获取用户组权限、获取角色权限】
 *      2. 修改表【t_user_attr】的【出生日期】 的数据类型为【DATE】
 *
 */

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('75a44e2d-e42b-4ec3-b4dc-a4c7a8362cad', '获取用户权限', 'permissions', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-10-26 18:36:02.055084', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('4f93643e-013b-46a2-8ca9-4316fc9b8d99', '获取用户组权限', 'permissions', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-10-26 18:36:30.025987', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('4193f726-31f4-40b1-b5d8-bf42f1ae9252', '获取角色权限', 'permissions', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-10-26 18:36:49.938818', 'admin', NULL, NULL, 1, 'f');

UPDATE "t_user_attr" SET "attr_data_type" = 'DATE' WHERE attr_id = '1c092d28-81fb-4e97-92ef-93d446c826c6';
