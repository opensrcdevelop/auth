/**
 * 变更：
 *      1. 表【t_user_attr】添加字段【user_visible, user_editable】
 *      2. 修改表【t_user_attr】的【创建时间】 的数据类型为【DATETIME】
 *
 */

ALTER TABLE "t_user_attr"
    ADD COLUMN "user_visible" bool DEFAULT true,
    ADD COLUMN "user_editable" bool DEFAULT true;

COMMENT ON COLUMN "t_user_attr"."user_visible" IS '用户可见';
COMMENT ON COLUMN "t_user_attr"."user_editable" IS '用户可编辑';

UPDATE "t_user_attr" SET "attr_data_type" = 'DATETIME' WHERE attr_id = '08604464-2832-44e3-8bd4-d8dbda7db9d7'
UPDATE "t_user_attr" SET "user_editable" = 'f', "user_visible" = 'f' WHERE attr_id IN ('6a5a3759-3fb3-47c3-bec6-f14d32e170c2', '47c3b7fb-fbce-4410-aa1e-3b1353468d49');
UPDATE "t_user_attr" SET "user_editable" = 'f', "user_visible" = 't' WHERE attr_id IN ('d019fb4e-8acd-4411-9061-9d8aee961703', '0965fca9-d005-4cd8-8a77-531d01b8fc05', '08604464-2832-44e3-8bd4-d8dbda7db9d7', '3845b5d4-36a0-45bb-854e-6d79593aefd4', 'ff289375-461b-4e6f-8e16-9187d7e44a14');
