/**
 * 变更：
 *      1. 删除唯一键：【t_dict_dict_code_key】、【t_client_client_name_key】、【t_permission_permission_code_resource_id_key】、【t_resource_resource_code_resource_group_id_key】、【t_resource_group_resource_group_code_key】、【t_role_role_code_key】
 *                    【t_user_email_address_key】、【t_user_username_key】、【t_user_phone_number_key】、【t_user_group_user_group_code_key】
 *
 */

 ALTER TABLE "t_dict" 
  DROP CONSTRAINT "t_dict_dict_code_key";

 ALTER TABLE "t_client" 
  DROP CONSTRAINT "t_client_client_name_key";

 ALTER TABLE "t_permission" 
  DROP CONSTRAINT "t_permission_permission_code_resource_id_key";

 ALTER TABLE "t_resource"
  DROP CONSTRAINT "t_resource_resource_code_resource_group_id_key";

 ALTER TABLE "t_resource_group"
  DROP CONSTRAINT "t_resource_group_resource_group_code_key";

 ALTER TABLE "t_role" 
  DROP CONSTRAINT "t_role_role_code_key";

 ALTER TABLE "t_user" 
  DROP CONSTRAINT "t_user_email_address_key",
  DROP CONSTRAINT "t_user_username_key",
  DROP CONSTRAINT "t_user_phone_number_key";

 ALTER TABLE "t_user_group" 
  DROP CONSTRAINT "t_user_group_user_group_code_key";
