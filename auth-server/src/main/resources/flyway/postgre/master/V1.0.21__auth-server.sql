/**
 * 变更：
 *      1. 表【t_dict】添加字段
 *      2. 表【t_permission】添加数据
 *
 */

-- ----------------------------
-- 添加字段
-- ----------------------------
ALTER TABLE "t_dict"
    DROP COLUMN IF EXISTS "parent_dict_id",
    DROP COLUMN IF EXISTS "related_dict_data_id",
    ADD COLUMN "parent_dict_id" varchar(50),
    ADD COLUMN "related_dict_data_id" varchar(50);

COMMENT ON COLUMN "t_dict"."parent_dict_id" IS '父字典ID';
COMMENT ON COLUMN "t_dict"."related_dict_data_id" IS '关联字典数据ID';

-- ----------------------------
-- 添加数据
-- ----------------------------
DELETE  FROM "t_permission" WHERE "permission_id" = '019b87ca-c7ab-7039-a3a5-4bf0a10ff77e';
DELETE  FROM "t_permission" WHERE "permission_id" = '019b87cb-12ce-7533-b50a-06e4a0829dc7';

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019b87ca-c7ab-7039-a3a5-4bf0a10ff77e', '添加子字典', 'addChildDict', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2026-01-04 14:56:12.204846', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('019b87cb-12ce-7533-b50a-06e4a0829dc7', '删除子字典', 'deleteChildDict', NULL, '22c26616-ba71-4258-a9b4-0901cc3285b7', '2026-01-04 14:56:31.439047', 'admin', NULL, NULL, 1, 'f');
