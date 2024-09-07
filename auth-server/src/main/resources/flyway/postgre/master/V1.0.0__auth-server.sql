/**
 * Auth Server Initialization Script
 */

-- ----------------------------
-- Table structure for t_authorization
-- ----------------------------
DROP TABLE IF EXISTS "t_authorization";

-- ----------------------------
-- Sequence structure for t_authorization_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_authorization_id_seq";
CREATE SEQUENCE "t_authorization_id_seq"
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_authorization" (
  "id" int8 NOT NULL DEFAULT nextval('t_authorization_id_seq'::regclass),
  "registered_client_id" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "principal_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "authorization_grant_type" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "authorized_scopes" varchar(500) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "attributes" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "state" varchar(500) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "authorization_code_value" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "authorization_code_issued_at" timestamp(6),
  "authorization_code_expires_at" timestamp(6),
  "authorization_code_metadata" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "access_token_value" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "access_token_issued_at" timestamp(6),
  "access_token_expires_at" timestamp(6),
  "access_token_metadata" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "access_token_type" varchar(255) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "access_token_scopes" varchar(500) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "refresh_token_value" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "refresh_token_issued_at" timestamp(6),
  "refresh_token_expires_at" timestamp(6),
  "refresh_token_metadata" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "oidc_id_token_value" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "oidc_id_token_issued_at" timestamp(6),
  "oidc_id_token_expires_at" timestamp(6),
  "oidc_id_token_metadata" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "oidc_id_token_claims" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "user_code_value" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "user_code_issued_at" timestamp(6),
  "user_code_expires_at" timestamp(6),
  "user_code_metadata" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "device_code_value" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "device_code_issued_at" timestamp(6),
  "device_code_expires_at" timestamp(6),
  "device_code_metadata" text COLLATE "pg_catalog"."default" DEFAULT NULL::character varying
)
;

-- ----------------------------
-- Table structure for t_authorize
-- ----------------------------
DROP TABLE IF EXISTS "t_authorize";

-- ----------------------------
-- Sequence structure for t_authority_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_authority_id_seq";
CREATE SEQUENCE "t_authority_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_authorize" (
  "id" int8 NOT NULL DEFAULT nextval('t_authority_id_seq'::regclass),
  "user_id" varchar(50) COLLATE "pg_catalog"."default",
  "role_id" varchar(50) COLLATE "pg_catalog"."default",
  "user_group_id" varchar(50) COLLATE "pg_catalog"."default",
  "permission_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "authorize_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "authorize_time" timestamp(6)
)
;
COMMENT ON COLUMN "t_authorize"."id" IS '主键';
COMMENT ON COLUMN "t_authorize"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_authorize"."role_id" IS '角色ID';
COMMENT ON COLUMN "t_authorize"."user_group_id" IS '用户组ID';
COMMENT ON COLUMN "t_authorize"."permission_id" IS '权限ID';
COMMENT ON COLUMN "t_authorize"."authorize_id" IS '授权ID';
COMMENT ON COLUMN "t_authorize"."authorize_time" IS '授权时间';

-- ----------------------------
-- Table structure for t_authorize_cond
-- ----------------------------
DROP TABLE IF EXISTS "t_authorize_cond";
CREATE TABLE "t_authorize_cond" (
  "authorize_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "permission_exp_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL
)
;
COMMENT ON COLUMN "t_authorize_cond"."authorize_id" IS '授权ID';
COMMENT ON COLUMN "t_authorize_cond"."permission_exp_id" IS '权限表达式ID';

-- ----------------------------
-- Table structure for t_client
-- ----------------------------
DROP TABLE IF EXISTS "t_client";

-- ----------------------------
-- Sequence structure for t_client_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_client_id_seq";
CREATE SEQUENCE "t_client_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_client" (
  "id" int8 NOT NULL DEFAULT nextval('t_client_id_seq'::regclass),
  "client_id" varchar(100) COLLATE "pg_catalog"."default" NOT NULL,
  "client_id_issued_at" timestamp(6) NOT NULL,
  "client_secret" varchar(200) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "client_secret_expires_at" timestamp(6),
  "client_name" varchar(200) COLLATE "pg_catalog"."default" NOT NULL,
  "client_authentication_methods" varchar(1000) COLLATE "pg_catalog"."default" NOT NULL,
  "authorization_grant_types" varchar(500) COLLATE "pg_catalog"."default" NOT NULL,
  "redirect_uris" varchar(1000) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "post_logout_redirect_uris" varchar(1000) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "scopes" varchar(500) COLLATE "pg_catalog"."default" NOT NULL,
  "client_settings" text COLLATE "pg_catalog"."default" NOT NULL,
  "token_settings" text COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Table structure for t_oidc_claim
-- ----------------------------
DROP TABLE IF EXISTS "t_oidc_claim";

-- ----------------------------
-- Sequence structure for t_oidc_claim_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_oidc_claim_id_seq";
CREATE SEQUENCE "t_oidc_claim_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_oidc_claim" (
  "id" int8 NOT NULL DEFAULT nextval('t_oidc_claim_id_seq'::regclass),
  "claim_id" varchar(50) COLLATE "pg_catalog"."default",
  "claim_name" varchar(500) COLLATE "pg_catalog"."default",
  "user_attr_id" varchar(50) COLLATE "pg_catalog"."default"
)
;
COMMENT ON COLUMN "t_oidc_claim"."id" IS '主键';
COMMENT ON COLUMN "t_oidc_claim"."claim_id" IS 'claim ID';
COMMENT ON COLUMN "t_oidc_claim"."claim_name" IS 'claim 名';
COMMENT ON COLUMN "t_oidc_claim"."user_attr_id" IS '用户属性ID';

-- ----------------------------
-- Table structure for t_oidc_claim_scope_mapping
-- ----------------------------
DROP TABLE IF EXISTS "t_oidc_claim_scope_mapping";
CREATE TABLE "t_oidc_claim_scope_mapping" (
  "claim_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "scope_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL
)
;

-- ----------------------------
-- Table structure for t_oidc_scope
-- ----------------------------
DROP TABLE IF EXISTS "t_oidc_scope";

-- ----------------------------
-- Sequence structure for t_scope_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_scope_id_seq";
CREATE SEQUENCE "t_scope_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_oidc_scope" (
  "id" int8 NOT NULL DEFAULT nextval('t_scope_id_seq'::regclass),
  "scope_id" varchar(50) COLLATE "pg_catalog"."default",
  "scope_name" varchar(500) COLLATE "pg_catalog"."default"
)
;
COMMENT ON COLUMN "t_oidc_scope"."id" IS '主键';
COMMENT ON COLUMN "t_oidc_scope"."scope_id" IS 'scope ID';
COMMENT ON COLUMN "t_oidc_scope"."scope_name" IS 'scope 名';

-- ----------------------------
-- Table structure for t_permission
-- ----------------------------
DROP TABLE IF EXISTS "t_permission";

-- ----------------------------
-- Sequence structure for t_permission_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_permission_id_seq";
CREATE SEQUENCE "t_permission_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_permission" (
  "id" int8 NOT NULL DEFAULT nextval('t_permission_id_seq'::regclass),
  "permission_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "permission_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "permission_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "resource_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_permission"."id" IS '主键';
COMMENT ON COLUMN "t_permission"."permission_id" IS '权限ID';
COMMENT ON COLUMN "t_permission"."permission_name" IS '权限名';
COMMENT ON COLUMN "t_permission"."permission_code" IS '权限码';
COMMENT ON COLUMN "t_permission"."description" IS '描述';
COMMENT ON COLUMN "t_permission"."resource_id" IS '资源ID';
COMMENT ON COLUMN "t_permission"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_permission"."create_by" IS '创建人';
COMMENT ON COLUMN "t_permission"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_permission"."update_by" IS '更新人';
COMMENT ON COLUMN "t_permission"."version" IS '版本';

-- ----------------------------
-- Table structure for t_permission_exp
-- ----------------------------
DROP TABLE IF EXISTS "t_permission_exp";

-- ----------------------------
-- Sequence structure for t_expression_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_expression_id_seq";
CREATE SEQUENCE "t_expression_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_permission_exp" (
  "id" int8 NOT NULL DEFAULT nextval('t_expression_id_seq'::regclass),
  "expression_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "expression_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "expression" varchar(1000) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_permission_exp"."id" IS '主键';
COMMENT ON COLUMN "t_permission_exp"."expression_id" IS '表达式ID';
COMMENT ON COLUMN "t_permission_exp"."expression_name" IS '表达式名称';
COMMENT ON COLUMN "t_permission_exp"."expression" IS '表达式';
COMMENT ON COLUMN "t_permission_exp"."description" IS '描述';

-- ----------------------------
-- Table structure for t_resource
-- ----------------------------
DROP TABLE IF EXISTS "t_resource";

-- ----------------------------
-- Sequence structure for t_resource_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_resource_id_seq";
CREATE SEQUENCE "t_resource_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_resource" (
  "id" int8 NOT NULL DEFAULT nextval('t_resource_id_seq'::regclass),
  "resource_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "resource_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "resource_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "resource_group_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "api_identifier" varchar(255) COLLATE "pg_catalog"."default",
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar COLLATE "pg_catalog"."default",
  "version" int4,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_resource"."id" IS '主键';
COMMENT ON COLUMN "t_resource"."resource_id" IS '资源ID';
COMMENT ON COLUMN "t_resource"."resource_name" IS '资源名称';
COMMENT ON COLUMN "t_resource"."resource_code" IS '资源码';
COMMENT ON COLUMN "t_resource"."description" IS '描述';
COMMENT ON COLUMN "t_resource"."api_identifier" IS 'API URL 标识';
COMMENT ON COLUMN "t_resource"."resource_group_id" IS '资源组ID';

-- ----------------------------
-- Table structure for t_resource_group
-- ----------------------------
DROP TABLE IF EXISTS "t_resource_group";

-- ----------------------------
-- Sequence structure for t_resource_group_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_resource_group_id_seq";
CREATE SEQUENCE "t_resource_group_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_resource_group" (
  "id" int8 NOT NULL DEFAULT nextval('t_resource_group_id_seq'::regclass),
  "resource_group_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "resource_group_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "resource_group_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_resource_group"."id" IS '主键';
COMMENT ON COLUMN "t_resource_group"."resource_group_id" IS '资源组ID';
COMMENT ON COLUMN "t_resource_group"."resource_group_name" IS '资源组名';
COMMENT ON COLUMN "t_resource_group"."resource_group_code" IS '资源组码';
COMMENT ON COLUMN "t_resource_group"."description" IS '描述';

-- ----------------------------
-- Table structure for t_role
-- ----------------------------
DROP TABLE IF EXISTS "t_role";

-- ----------------------------
-- Sequence structure for t_role_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_role_id_seq";
CREATE SEQUENCE "t_role_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_role" (
  "id" int8 NOT NULL DEFAULT nextval('t_role_id_seq'::regclass),
  "role_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "role_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "role_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(1000) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_role"."id" IS '主键';
COMMENT ON COLUMN "t_role"."role_id" IS '角色ID';
COMMENT ON COLUMN "t_role"."role_name" IS '角色名称';
COMMENT ON COLUMN "t_role"."role_code" IS '角色码';
COMMENT ON COLUMN "t_role"."description" IS '描述';

-- ----------------------------
-- Table structure for t_role_mapping
-- ----------------------------
DROP TABLE IF EXISTS "t_role_mapping";
CREATE TABLE "t_role_mapping" (
  "user_id" varchar(50) COLLATE "pg_catalog"."default",
  "role_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "user_group_id" varchar(50) COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for t_user
-- ----------------------------
DROP TABLE IF EXISTS "t_user";

-- ----------------------------
-- Sequence structure for t_user_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_user_id_seq";
CREATE SEQUENCE "t_user_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_user" (
  "id" int8 NOT NULL DEFAULT nextval('t_user_id_seq'::regclass),
  "user_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "username" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "password" varchar(500) COLLATE "pg_catalog"."default" NOT NULL,
  "phone_number" varchar(50) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "email_address" varchar(50) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "enable_mfa" bool DEFAULT false,
  "mfa_secret" varchar(500) COLLATE "pg_catalog"."default" DEFAULT NULL::character varying,
  "mfa_device_bind" bool,
  "need_change_pwd" bool DEFAULT false,
  "locked" bool DEFAULT false,
  "console_access" bool DEFAULT false,
  "last_login_time" timestamp(6),
  "last_login_ip" varchar(50) COLLATE "pg_catalog"."default",
  "last_login_device_type" varchar(50) COLLATE "pg_catalog"."default",
  "last_login_device_os" varchar(50) COLLATE "pg_catalog"."default",
  "login_failed_cnt" int4,
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int2 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_user"."id" IS '主键';
COMMENT ON COLUMN "t_user"."user_id" IS '用户 ID';
COMMENT ON COLUMN "t_user"."username" IS '用户名';
COMMENT ON COLUMN "t_user"."password" IS '密码';
COMMENT ON COLUMN "t_user"."phone_number" IS '手机号码';
COMMENT ON COLUMN "t_user"."email_address" IS '邮箱地址';
COMMENT ON COLUMN "t_user"."enable_mfa" IS '启用 MFA';
COMMENT ON COLUMN "t_user"."mfa_secret" IS 'MFA 密钥';
COMMENT ON COLUMN "t_user"."mfa_device_bind" IS 'MFA 设备绑定状态';
COMMENT ON COLUMN "t_user"."need_change_pwd" IS '需要变更密码';
COMMENT ON COLUMN "t_user"."locked" IS '禁用账号';
COMMENT ON COLUMN "t_user"."console_access" IS '控制台访问';
COMMENT ON COLUMN "t_user"."last_login_time" IS '上次登录时间';
COMMENT ON COLUMN "t_user"."last_login_ip" IS '上次登录 IP';
COMMENT ON COLUMN "t_user"."last_login_device_type" IS '上次登录设备类型';
COMMENT ON COLUMN "t_user"."last_login_device_os" IS '上次登录设备操作系统';
COMMENT ON COLUMN "t_user"."login_failed_cnt" IS '登录失败次数';
COMMENT ON COLUMN "t_user"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_user"."create_by" IS '创建人';
COMMENT ON COLUMN "t_user"."update_time" IS '更新时间';
COMMENT ON COLUMN "t_user"."update_by" IS '更新人';
COMMENT ON COLUMN "t_user"."version" IS '版本';
COMMENT ON COLUMN "t_user"."deleted" IS '逻辑删除';

-- ----------------------------
-- Table structure for t_user_attr
-- ----------------------------
DROP TABLE IF EXISTS "t_user_attr";

-- ----------------------------
-- Sequence structure for t_user_attr_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_user_attr_id_seq";
CREATE SEQUENCE "t_user_attr_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_user_attr" (
  "id" int8 NOT NULL DEFAULT nextval('t_user_attr_id_seq'::regclass),
  "attr_id" varchar(255) COLLATE "pg_catalog"."default",
  "attr_key" varchar(255) COLLATE "pg_catalog"."default",
  "attr_name" varchar(255) COLLATE "pg_catalog"."default",
  "attr_data_type" varchar(20) COLLATE "pg_catalog"."default",
  "ext_attr_flg" bool DEFAULT true,
  "user_lst_display" bool DEFAULT true,
  "display_seq" int4,
  "display_width" int4
)
;
COMMENT ON COLUMN "t_user_attr"."id" IS '主键';
COMMENT ON COLUMN "t_user_attr"."attr_id" IS '属性ID';
COMMENT ON COLUMN "t_user_attr"."attr_key" IS '属性键';
COMMENT ON COLUMN "t_user_attr"."attr_name" IS '属性名';
COMMENT ON COLUMN "t_user_attr"."attr_data_type" IS '属性数据类型';
COMMENT ON COLUMN "t_user_attr"."ext_attr_flg" IS '扩展属性标记';
COMMENT ON COLUMN "t_user_attr"."user_lst_display" IS '是否在用户列表显示';
COMMENT ON COLUMN "t_user_attr"."display_seq" IS '显示顺序';
COMMENT ON COLUMN "t_user_attr"."display_width" IS '显示宽度';

-- ----------------------------
-- Table structure for t_user_attr_mapping
-- ----------------------------
DROP TABLE IF EXISTS "t_user_attr_mapping";
CREATE TABLE "t_user_attr_mapping" (
  "user_id" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "attr_id" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "attr_value" varchar(2000) COLLATE "pg_catalog"."default"
)
;

-- ----------------------------
-- Table structure for t_user_group
-- ----------------------------
DROP TABLE IF EXISTS "t_user_group";

-- ----------------------------
-- Sequence structure for t_user_group_id_seq
-- ----------------------------
DROP SEQUENCE IF EXISTS "t_user_group_id_seq";
CREATE SEQUENCE "t_user_group_id_seq" 
INCREMENT 1
MINVALUE  1
MAXVALUE 9223372036854775807
START 1
CACHE 1;

CREATE TABLE "t_user_group" (
  "id" int8 NOT NULL DEFAULT nextval('t_user_group_id_seq'::regclass),
  "user_group_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "user_group_name" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "user_group_code" varchar(255) COLLATE "pg_catalog"."default" NOT NULL,
  "description" varchar(100) COLLATE "pg_catalog"."default",
  "create_time" timestamp(6),
  "create_by" varchar(255) COLLATE "pg_catalog"."default",
  "update_time" timestamp(6),
  "update_by" varchar(255) COLLATE "pg_catalog"."default",
  "version" int4 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;
COMMENT ON COLUMN "t_user_group"."id" IS '主键';
COMMENT ON COLUMN "t_user_group"."user_group_id" IS '用户组ID';
COMMENT ON COLUMN "t_user_group"."user_group_name" IS '用户组名';
COMMENT ON COLUMN "t_user_group"."user_group_code" IS '用户组码';
COMMENT ON COLUMN "t_user_group"."description" IS '描述';

-- ----------------------------
-- Table structure for t_user_group_mapping
-- ----------------------------
DROP TABLE IF EXISTS "t_user_group_mapping";
CREATE TABLE "t_user_group_mapping" (
  "user_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL,
  "user_group_id" varchar(50) COLLATE "pg_catalog"."default" NOT NULL
)
;

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_authority_id_seq"
OWNED BY "t_authorize"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_authorization_id_seq"
OWNED BY "t_authorization"."id";
-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_client_id_seq"
OWNED BY "t_client"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_expression_id_seq"
OWNED BY "t_permission_exp"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_oidc_claim_id_seq"
OWNED BY "t_oidc_claim"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_permission_id_seq"
OWNED BY "t_permission"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_resource_group_id_seq"
OWNED BY "t_resource_group"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_resource_id_seq"
OWNED BY "t_resource"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_role_id_seq"
OWNED BY "t_role"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_scope_id_seq"
OWNED BY "t_oidc_scope"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_user_attr_id_seq"
OWNED BY "t_user_attr"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_user_group_id_seq"
OWNED BY "t_user_group"."id";

-- ----------------------------
-- Alter sequences owned by
-- ----------------------------
ALTER SEQUENCE "t_user_id_seq"
OWNED BY "t_user"."id";

-- ----------------------------
-- Primary Key structure for table t_authorization
-- ----------------------------
ALTER TABLE "t_authorization" ADD CONSTRAINT "t_authorization_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table t_authorize
-- ----------------------------
ALTER TABLE "t_authorize" ADD CONSTRAINT "t_authority_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table t_authorize_cond
-- ----------------------------
ALTER TABLE "t_authorize_cond" ADD CONSTRAINT "t_authorize_cond_pkey" PRIMARY KEY ("authorize_id", "permission_exp_id");

-- ----------------------------
-- Uniques structure for table t_client
-- ----------------------------
ALTER TABLE "t_client" ADD CONSTRAINT "t_client_client_name_key" UNIQUE ("client_name");

-- ----------------------------
-- Primary Key structure for table t_client
-- ----------------------------
ALTER TABLE "t_client" ADD CONSTRAINT "t_client_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Uniques structure for table t_oidc_claim
-- ----------------------------
ALTER TABLE "t_oidc_claim" ADD CONSTRAINT "t_oidc_claim_claim_name_key" UNIQUE ("claim_name");

-- ----------------------------
-- Primary Key structure for table t_oidc_claim
-- ----------------------------
ALTER TABLE "t_oidc_claim" ADD CONSTRAINT "t_oidc_claim_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table t_oidc_claim_scope_mapping
-- ----------------------------
ALTER TABLE "t_oidc_claim_scope_mapping" ADD CONSTRAINT "t_oidc_claim_scope_mapping_pkey" PRIMARY KEY ("claim_id", "scope_id");

-- ----------------------------
-- Uniques structure for table t_oidc_scope
-- ----------------------------
ALTER TABLE "t_oidc_scope" ADD CONSTRAINT "t_oidc_scope_scope_name_key" UNIQUE ("scope_name");

-- ----------------------------
-- Primary Key structure for table t_oidc_scope
-- ----------------------------
ALTER TABLE "t_oidc_scope" ADD CONSTRAINT "t_scope_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Uniques structure for table t_permission
-- ----------------------------
ALTER TABLE "t_permission" ADD CONSTRAINT "t_permission_permission_code_resource_id_key" UNIQUE ("permission_code", "resource_id");

-- ----------------------------
-- Primary Key structure for table t_permission
-- ----------------------------
ALTER TABLE "t_permission" ADD CONSTRAINT "t_permission_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table t_permission_exp
-- ----------------------------
ALTER TABLE "t_permission_exp" ADD CONSTRAINT "t_expression_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Uniques structure for table t_resource
-- ----------------------------
ALTER TABLE "t_resource" ADD CONSTRAINT "t_resource_resource_code_resource_group_id_key" UNIQUE ("resource_code", "resource_group_id");

-- ----------------------------
-- Primary Key structure for table t_resource
-- ----------------------------
ALTER TABLE "t_resource" ADD CONSTRAINT "t_resource_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Uniques structure for table t_resource_group
-- ----------------------------
ALTER TABLE "t_resource_group" ADD CONSTRAINT "t_resource_group_resource_group_code_key" UNIQUE ("resource_group_code");

-- ----------------------------
-- Primary Key structure for table t_resource_group
-- ----------------------------
ALTER TABLE "t_resource_group" ADD CONSTRAINT "t_resource_group_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Uniques structure for table t_role
-- ----------------------------
ALTER TABLE "t_role" ADD CONSTRAINT "t_role_role_code_key" UNIQUE ("role_code");

-- ----------------------------
-- Primary Key structure for table t_role
-- ----------------------------
ALTER TABLE "t_role" ADD CONSTRAINT "t_role_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Uniques structure for table t_user
-- ----------------------------
ALTER TABLE "t_user" ADD CONSTRAINT "t_user_user_id_key" UNIQUE ("user_id");
ALTER TABLE "t_user" ADD CONSTRAINT "t_user_email_address_key" UNIQUE ("email_address");
ALTER TABLE "t_user" ADD CONSTRAINT "t_user_username_key" UNIQUE ("username");
ALTER TABLE "t_user" ADD CONSTRAINT "t_user_phone_number_key" UNIQUE ("phone_number");

-- ----------------------------
-- Primary Key structure for table t_user
-- ----------------------------
ALTER TABLE "t_user" ADD CONSTRAINT "t_user_pk" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table t_user_attr
-- ----------------------------
ALTER TABLE "t_user_attr" ADD CONSTRAINT "t_user_attr_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table t_user_attr_mapping
-- ----------------------------
ALTER TABLE "t_user_attr_mapping" ADD CONSTRAINT "t_user_user_attr_mapping_pkey" PRIMARY KEY ("user_id", "attr_id");

-- ----------------------------
-- Uniques structure for table t_user_group
-- ----------------------------
ALTER TABLE "t_user_group" ADD CONSTRAINT "t_user_group_user_group_code_key" UNIQUE ("user_group_code");
ALTER TABLE "t_user_group" ADD CONSTRAINT "t_user_group_user_group_id_key" UNIQUE ("user_group_id");

-- ----------------------------
-- Primary Key structure for table t_user_group
-- ----------------------------
ALTER TABLE "t_user_group" ADD CONSTRAINT "t_user_group_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Primary Key structure for table t_user_group_mapping
-- ----------------------------
ALTER TABLE "t_user_group_mapping" ADD CONSTRAINT "t_user_user_group_mapping_pkey" PRIMARY KEY ("user_id", "user_group_id");

-- ----------------------------
-- Records of t_client
-- ----------------------------
INSERT INTO "t_client" ("client_id", "client_id_issued_at", "client_secret", "client_secret_expires_at", "client_name", "client_authentication_methods", "authorization_grant_types", "redirect_uris", "post_logout_redirect_uris", "scopes", "client_settings", "token_settings", "create_time", "create_by", "update_time", "update_by", "version", "description", "deleted") VALUES ('52cb8d26-a352-4e5c-99a7-d52b8afff3b1', '2024-08-14 00:26:47.047908', '$2a$10$9pTi/6V2FRWyH6MkgBNlL.GeDxDWWt99xQlvY1qhbxxbywOhWmpka', NULL, 'auth-server-console', 'client_secret_post,client_secret_basic', 'refresh_token,client_credentials,password,authorization_code', '${consoleRedirectUrl}', '', 'openid', '{"@class":"java.util.HashMap","settings.client.require-proof-key":false,"settings.client.require-authorization-consent":false}', '{"@class":"java.util.HashMap","settings.token.reuse-refresh-tokens":true,"settings.token.id-token-signature-algorithm":["org.springframework.security.oauth2.jose.jws.SignatureAlgorithm","RS256"],"settings.token.access-token-time-to-live":["java.time.Duration",14400.000000000],"settings.token.access-token-format":{"@class":"org.springframework.security.oauth2.server.authorization.settings.OAuth2TokenFormat","value":"self-contained"},"settings.token.refresh-token-time-to-live":["java.time.Duration",604800.000000000],"settings.token.authorization-code-time-to-live":["java.time.Duration",300.000000000],"settings.token.device-code-time-to-live":["java.time.Duration",300.000000000]}', '2024-08-14 00:26:47.113549', 'admin', NULL, NULL, 1, '认证授权服务控制台', 'f');

-- ----------------------------
-- Records of t_oidc_claim
-- ----------------------------
INSERT INTO "t_oidc_claim" ("claim_id", "claim_name", "user_attr_id") VALUES ('e7ea62f4-e672-4412-b2e1-e42349520845', 'username', '3cb048e2-5896-46aa-96e6-fa975b4780f5');
INSERT INTO "t_oidc_claim" ("claim_id", "claim_name", "user_attr_id") VALUES ('065e5264-d478-4c05-8d3f-411f99a114b2', 'sub', 'd019fb4e-8acd-4411-9061-9d8aee961703');
INSERT INTO "t_oidc_claim" ("claim_id", "claim_name", "user_attr_id") VALUES ('5f1586a3-f751-4fa4-bd84-9925c3121087', 'phone', '3845b5d4-36a0-45bb-854e-6d79593aefd4');
INSERT INTO "t_oidc_claim" ("claim_id", "claim_name", "user_attr_id") VALUES ('bb0a09f7-268f-4afd-8d2f-69bebd7d01e4', 'email', 'ff289375-461b-4e6f-8e16-9187d7e44a14');

-- ----------------------------
-- Records of t_oidc_claim_scope_mapping
-- ----------------------------
INSERT INTO "t_oidc_claim_scope_mapping" ("claim_id", "scope_id") VALUES ('bb0a09f7-268f-4afd-8d2f-69bebd7d01e4', 'fc3ef9e9-ef3a-445d-acdc-a6fcb247348f');
INSERT INTO "t_oidc_claim_scope_mapping" ("claim_id", "scope_id") VALUES ('5f1586a3-f751-4fa4-bd84-9925c3121087', 'fc3ef9e9-ef3a-445d-acdc-a6fcb247348f');
INSERT INTO "t_oidc_claim_scope_mapping" ("claim_id", "scope_id") VALUES ('065e5264-d478-4c05-8d3f-411f99a114b2', '570c7859-8a40-4688-b8a2-3352438f4ea5');
INSERT INTO "t_oidc_claim_scope_mapping" ("claim_id", "scope_id") VALUES ('e7ea62f4-e672-4412-b2e1-e42349520845', '570c7859-8a40-4688-b8a2-3352438f4ea5');

-- ----------------------------
-- Records of t_oidc_scope
-- ----------------------------
INSERT INTO "t_oidc_scope" ("scope_id", "scope_name") VALUES ('fc3ef9e9-ef3a-445d-acdc-a6fcb247348f', 'profile');
INSERT INTO "t_oidc_scope" ("scope_id", "scope_name") VALUES ('570c7859-8a40-4688-b8a2-3352438f4ea5', 'openid');

-- ----------------------------
-- Records of t_permission
-- ----------------------------
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('c4395ddb-0ffc-4926-b695-7f0e7c923a31', '所有权限', 'all', NULL, '14ee7b7e-db4c-40cc-b93e-d38969be5542', '2024-08-14 20:53:19.745444', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('a61250fc-2137-47ee-af75-89e818284a16', '所有权限', 'all', NULL, '75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '2024-08-14 20:53:28.953714', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('a4864949-8375-44bb-8c89-162a90dae9fa', '所有权限', 'all', NULL, '931848a2-beb6-444e-a7d7-72e909553b00', '2024-08-14 20:53:39.998839', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('1382716a-f327-41bf-89c6-dd096b35c636', '所有权限', 'all', NULL, '0f35efeb-3f5a-4e22-84c4-4b4a08b6717c', '2024-08-14 20:53:50.957663', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('09ab38a0-2cf7-4394-b69f-5039826b6f6e', '所有权限', 'all', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2024-08-14 20:54:10.162655', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('7001267e-b258-4c78-81e9-b029c5e35169', '所有权限', 'all', NULL, '79a30d3a-0fde-4087-a307-619cc0c56b17', '2024-08-14 20:54:19.659864', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('47c8a3e9-f822-40a0-9681-d9e19bda7039', '所有权限', 'all', NULL, '911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '2024-08-14 20:54:41.369872', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('cb6b3296-1885-43e8-aef9-3e3cbcf47dc2', '所有权限', 'all', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-08-14 20:54:52.619629', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0e27fb8b-d3fa-43e8-acfa-4f744e1e9a38', '所有权限', 'all', NULL, 'df35c2ee-f8fb-4a3e-8627-879d2bcd23cc', '2024-08-14 20:55:06.934151', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('6fc2e212-ce2d-4248-80ce-2850902a8e63', '所有权限', 'all', NULL, 'da2c6573-d236-4e4d-96a4-85c517b72c59', '2024-08-14 20:55:46.811952', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('4d936c24-e90b-4fc7-b201-8830a46496dc', '所有权限', 'all', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-14 20:55:30.692998', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('261ece17-1a3a-442c-b37a-b8bf0b849591', '所有权限', 'all', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-08-14 20:56:00.946298', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b4c95f7f-77ea-432a-8d33-02073dfa14d4', '所有权限', 'all', NULL, '6df389ec-09be-4443-a80a-b3fed5d9b9d8', '2024-08-14 20:56:12.742177', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ea490762-fbcb-48df-99e9-9e7ccc3f8805', '获取用户列表', 'list', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-14 22:22:43.630697', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('fb2197fc-9043-421d-a69a-33d1a38ffd48', '更新用户', 'update', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-14 22:22:57.919023', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5a06c504-6f1d-4cf7-b05e-509d66b67dbf', '获取用户详情', 'detail', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-14 22:23:10.354773', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('59ecdac1-8cd2-4edf-a1b2-54db15771a10', '删除用户', 'delete', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-14 22:23:23.181876', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b8942048-c809-405f-bcc7-107b987288a1', '创建用户属性', 'create', NULL, 'da2c6573-d236-4e4d-96a4-85c517b72c59', '2024-08-14 22:23:56.737658', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('1afe2798-19ab-45a4-968f-e623bba40abf', '更新用户属性', 'update', NULL, 'da2c6573-d236-4e4d-96a4-85c517b72c59', '2024-08-14 22:24:30.346887', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('59e1e18d-9730-4daf-98a9-71fc92d699a4', '设置用户属性显示顺序', 'setDisplaySeq', NULL, 'da2c6573-d236-4e4d-96a4-85c517b72c59', '2024-08-14 22:24:58.829358', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('4fae2e4e-122e-4d65-80d6-5d9ec9bebbb3', '获取用户属性详情', 'detail', NULL, 'da2c6573-d236-4e4d-96a4-85c517b72c59', '2024-08-14 22:25:27.044132', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b0a23fa1-b92d-4014-bf26-adede404cb61', '删除用户属性', 'delete', NULL, 'da2c6573-d236-4e4d-96a4-85c517b72c59', '2024-08-14 22:25:40.199076', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('05144706-d5d7-4aa8-bc98-e358303e1c9c', '创建客户端', 'create', NULL, '75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '2024-08-14 22:26:18.226828', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('7176aeeb-ba59-4791-a37e-36e668d009c8', '获取客户端列表', 'list', NULL, '75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '2024-08-14 22:26:32.16908', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('6f3b915e-bcb5-4174-86c0-9e84fab517ff', '获取客户端详情', 'detail', NULL, '75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '2024-08-14 22:26:44.037065', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5bfd26a3-8e87-4969-8f37-2b61ef3ca835', '更新客户端密钥', 'updateSecret', NULL, '75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '2024-08-14 22:27:05.196392', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('570d31c5-5125-4a92-b96c-ac2caa381dc4', '更新客户端', 'update', NULL, '75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '2024-08-14 22:27:27.67589', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('2646f161-ec7a-46e2-8a33-607167cca4db', '删除客户端', 'delete', NULL, '75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '2024-08-14 22:27:37.870147', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('3c20248a-1429-4d93-a6e3-818197ebb256', '创建 OIDC Scope', 'create', NULL, '0f35efeb-3f5a-4e22-84c4-4b4a08b6717c', '2024-08-14 22:28:12.113561', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5eb579f9-369a-47cf-b220-85761cb9f1ed', '获取 OIDC Scope 列表', 'list', NULL, '0f35efeb-3f5a-4e22-84c4-4b4a08b6717c', '2024-08-14 22:28:35.036941', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('8d860e88-bcdf-45f6-a52f-85794ccfa616', '更新 OIDC Scope', 'update', NULL, '0f35efeb-3f5a-4e22-84c4-4b4a08b6717c', '2024-08-14 22:29:13.253649', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('a3f90e89-8fe2-4afb-83c1-7d099de5f091', '删除 OIDC Scope', 'delete', NULL, '0f35efeb-3f5a-4e22-84c4-4b4a08b6717c', '2024-08-14 22:29:27.056353', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('69817bef-17d9-469c-bd7c-069c0cd26ff7', '获取 OIDC Claim 列表', 'list', NULL, '931848a2-beb6-444e-a7d7-72e909553b00', '2024-08-14 22:30:13.094575', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b74526ee-d553-47e7-8e45-4611bb95bda5', '更新 OIDC Claim', 'update', NULL, '931848a2-beb6-444e-a7d7-72e909553b00', '2024-08-14 22:30:34.173884', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('2c7202e1-9500-4201-8a06-7a64204b8fca', '删除 OIDC Claim', 'delete', NULL, '931848a2-beb6-444e-a7d7-72e909553b00', '2024-08-14 22:30:48.469687', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('4c4ca4b2-8da6-4c2d-8386-c1e9725f7069', '创建权限', 'create', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2024-08-14 22:31:22.996192', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('17ef1d22-f669-4057-ab3a-8027ba6289a8', '权限授权', 'authorize', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2024-08-14 22:31:40.689475', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('72c4286c-ace4-46f9-9263-9c1dea42eed1', '取消授权', 'unAuthorize', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2024-08-14 22:31:57.48902', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b3a4d646-5c60-4ae9-ab9c-d5c1fbc0e39b', '获取权限详情', 'detail', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2024-08-14 22:32:16.825172', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5bfecf5d-012e-4ccf-ac9c-20d8fc807829', '删除权限', 'delete', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2024-08-14 22:32:30.380261', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('1e56db18-3720-4991-8c68-90fbc408cfab', '更新权限', 'update', NULL, '75c35dc3-1996-48ab-be27-e4078f86a559', '2024-08-14 22:32:44.002688', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('252871e6-512d-4f6a-9756-c5bac5d599ce', '创建权限表达式', 'create', NULL, '79a30d3a-0fde-4087-a307-619cc0c56b17', '2024-08-14 22:33:33.940823', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('c73f2f07-0cc5-4d9e-99ec-8b1686686673', '获取权限表达式列表', 'list', NULL, '79a30d3a-0fde-4087-a307-619cc0c56b17', '2024-08-14 22:33:48.437197', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f3a06b3e-66e5-4aad-bb9a-8cc653bb260c', '获取权限表达式详情', 'detail', NULL, '79a30d3a-0fde-4087-a307-619cc0c56b17', '2024-08-14 22:34:01.888189', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5d7e6ab1-97a3-4ab7-8a5c-78ded02a17b7', '更新权限表达式', 'update', NULL, '79a30d3a-0fde-4087-a307-619cc0c56b17', '2024-08-14 22:34:17.397148', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('749b535e-568e-4d5d-81bf-715de2ad4684', '添加授权条件', 'add', NULL, '14ee7b7e-db4c-40cc-b93e-d38969be5542', '2024-08-14 22:34:58.907723', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('a221f9c1-60d9-4f9f-86c0-4aa8ee4c10fb', '删除授权条件', 'delete', NULL, '14ee7b7e-db4c-40cc-b93e-d38969be5542', '2024-08-14 22:35:08.78791', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f7e81888-53d1-4fd7-b645-29ffa989d8e4', '创建资源', 'create', NULL, 'edd2a541-f482-45cd-9842-c1ebf43c346c', '2024-08-14 22:35:38.612655', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b6ccd8cc-fcac-4e19-ac91-e0101a6d1574', '获取资源详情', 'detail', NULL, 'edd2a541-f482-45cd-9842-c1ebf43c346c', '2024-08-14 22:36:11.566715', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b17004d2-4499-4982-8e92-1b77c7786b7f', '获取资源内权限', 'getPermissions', NULL, 'edd2a541-f482-45cd-9842-c1ebf43c346c', '2024-08-14 22:36:35.35045', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('aeeb1509-9a49-4394-8a97-f07ca87d9ec5', '更新资源', 'update', NULL, 'edd2a541-f482-45cd-9842-c1ebf43c346c', '2024-08-14 22:36:53.33113', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('17f60b95-fe38-4d8e-9df7-aef2a0176c09', '创建用户', 'create', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-14 01:00:27.598613', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('7819e0f4-3ebd-4ce7-ad8d-304e485eff03', '所有权限', 'all', NULL, 'edd2a541-f482-45cd-9842-c1ebf43c346c', '2024-08-14 20:54:32.065378', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('3bd98f8d-ee8f-4b41-a83f-76d347782088', '获取用户属性列表', 'list', NULL, 'da2c6573-d236-4e4d-96a4-85c517b72c59', '2024-08-14 22:24:14.00862', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f6c48bbc-e457-4ca7-bd42-093c3a4b387a', '创建 OIDC Claim', 'create', NULL, '931848a2-beb6-444e-a7d7-72e909553b00', '2024-08-14 22:29:55.241751', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('e2cf8857-d549-4c64-858c-ba847ab9afd7', '删除权限表达式', 'delete', NULL, '79a30d3a-0fde-4087-a307-619cc0c56b17', '2024-08-14 22:34:26.503472', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ef6f144b-2642-4992-8554-39b250158cb6', '获取资源列表', 'list', NULL, 'edd2a541-f482-45cd-9842-c1ebf43c346c', '2024-08-14 22:35:57.43341', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f4a64ac7-3280-4594-92ba-536ac4639181', '删除资源', 'delete', NULL, 'edd2a541-f482-45cd-9842-c1ebf43c346c', '2024-08-14 22:37:01.13092', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('abc8e4c1-6f42-49e1-808b-089c7219c460', '创建资源组', 'create', NULL, '911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '2024-08-14 22:37:26.313743', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('9b528be9-d4ba-424e-bcce-33f50d611ca4', '获取资源组详情', 'detail', NULL, '911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '2024-08-14 22:41:44.25261', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('adc10f8a-6a9e-4085-834b-dddcb5e7d7bf', '更新资源组', 'update', NULL, '911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '2024-08-14 22:42:11.276355', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('5bba4e63-cdce-4c71-b226-b275d23fab77', '删除资源组', 'delete', NULL, '911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '2024-08-14 22:42:20.216852', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('84dd188d-5f5e-4aeb-9858-645bfa3dd041', '创建角色', 'create', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-08-14 22:42:51.968763', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ee27dd14-2e96-4407-9a19-23ef725624f4', '获取角色列表', 'list', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-08-14 22:43:04.348769', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b9cbf6d1-2828-4b6b-a9e1-275a1730f752', '获取角色主体', 'getPrincipals', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-08-14 22:43:15.906407', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('4ff05ce8-4851-40e2-98bf-1def8633a3b8', '获取角色详情', 'detail', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-08-14 22:43:27.712383', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0207cc07-ec87-4066-a49a-5397fddc0718', '更新角色', 'update', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-08-14 22:43:37.223058', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('1d725b56-fef0-4cad-92e3-1e4b000c470e', '删除角色', 'delete', NULL, '4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '2024-08-14 22:43:45.432588', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('ac41c9eb-c1eb-4408-b9a2-370055952798', '创建角色映射', 'create', NULL, 'df35c2ee-f8fb-4a3e-8627-879d2bcd23cc', '2024-08-14 22:44:20.117601', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('fdf62975-8f49-4f19-92af-b5224da8b8a3', '删除角色映射', 'delete', NULL, 'df35c2ee-f8fb-4a3e-8627-879d2bcd23cc', '2024-08-14 22:44:30.701136', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('a25c4da8-e54d-4088-9819-cdfe891df62f', '创建用户组', 'create', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-08-14 22:46:37.233224', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('8ad27151-f8ba-40c8-95b9-ff2161676473', '获取用户组列表', 'list', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-08-14 22:46:50.183101', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('8c67005b-b48c-4f17-a42d-90a6e2811e3f', '获取用户组详情', 'detail', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-08-14 22:47:09.082777', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('447e5ba3-c4f9-4e63-9815-3816fb40a1ac', '获取用户组内用户', 'getUsers', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-08-14 22:47:26.033877', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('441f3fc9-8146-4bc2-a819-c226ab54bdb3', '更新用户组', 'update', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-08-14 22:47:45.733524', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('9d8a07d8-7b6b-4a3c-b869-24e036f4b8c8', '删除用户组', 'delete', NULL, '1624ca73-e656-48d9-800e-b5762b51d7c5', '2024-08-14 22:48:15.425003', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('7f40fa0a-fae0-475e-835b-803e36e70ac9', '创建用户组映射', 'create', NULL, '6df389ec-09be-4443-a80a-b3fed5d9b9d8', '2024-08-14 22:48:45.998887', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('cf29d8ac-f4b5-46e6-af44-6cb795ced9f5', '删除用户组映射', 'delete', NULL, '6df389ec-09be-4443-a80a-b3fed5d9b9d8', '2024-08-14 22:48:55.05974', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('cf374bd6-6d02-4e0b-83ce-85e8ff6edec5', '获取资源组内资源', 'getResources', NULL, '911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '2024-08-14 22:42:00.156475', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f0ae03c7-a9f1-4f92-acf2-7303e60b6fd0', '获取资源组列表', 'list', NULL, '911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '2024-08-14 22:38:13.8398', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('649fad42-fa6a-4b1f-9cc0-421c24ee8619', '重新绑定 MFA 设备', 'rebindMfaDevice', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-17 14:34:08.829293', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('c80234f3-f7ec-4afc-ac26-b291f9bd59ce', '清除授权的 Token', 'clearTokens', NULL, '97392350-5214-4dbb-83e8-45b678ce145e', '2024-08-18 16:47:25.071121', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_resource
-- ----------------------------
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('da2c6573-d236-4e4d-96a4-85c517b72c59', '用户属性', 'userAttr', NULL, '/api/v1/user/attr', '2024-08-14 01:03:57.842057', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('97392350-5214-4dbb-83e8-45b678ce145e', '用户', 'user', NULL, '/api/v1/user', '2024-08-14 01:00:09.26225', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('14ee7b7e-db4c-40cc-b93e-d38969be5542', '授权条件', 'authorizeCond', NULL, '/api/v1/permission/authorize/cond', '2024-08-14 20:46:31.111148', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('75a3dee9-a95f-4ad3-a32a-a7f6d34c0050', '客户端', 'client', NULL, '/api/v1/client', '2024-08-14 20:45:38.663567', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('0f35efeb-3f5a-4e22-84c4-4b4a08b6717c', 'OIDC Scope', 'oidcScope', NULL, '/api/v1/oidc/scope', '2024-08-14 20:45:58.954905', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('931848a2-beb6-444e-a7d7-72e909553b00', 'OIDC Claim', 'oidcClaim', NULL, '/api/v1/oidc/claim', '2024-08-14 20:46:08.764515', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('75c35dc3-1996-48ab-be27-e4078f86a559', '权限', 'permission', NULL, '/api/v1/permission', '2024-08-14 20:46:15.791123', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('79a30d3a-0fde-4087-a307-619cc0c56b17', '权限表达式', 'permissionExp', NULL, '/api/v1/permission/exp', '2024-08-14 20:46:23.853985', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('edd2a541-f482-45cd-9842-c1ebf43c346c', '资源', 'resource', NULL, '/api/v1/resource', '2024-08-14 20:46:41.03735', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('911e08a0-d91a-4c66-8a7d-c8fda2c79c69', '资源组', 'resourceGroup', NULL, '/api/v1/resourceGroup', '2024-08-14 20:46:48.66517', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('4d367bc0-d043-402c-a1d5-d4e5c55c9e23', '角色', 'role', NULL, '/api/v1/role', '2024-08-14 20:46:56.62956', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('df35c2ee-f8fb-4a3e-8627-879d2bcd23cc', '角色映射', 'roleMapping', NULL, '/api/v1/role/mapping', '2024-08-14 20:47:08.624134', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('1624ca73-e656-48d9-800e-b5762b51d7c5', '用户组', 'userGroup', NULL, '/api/v1/userGroup', '2024-08-14 20:47:19.557369', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "description", "api_identifier", "create_time", "create_by", "update_time", "update_by", "version", "resource_group_id", "deleted") VALUES ('6df389ec-09be-4443-a80a-b3fed5d9b9d8', '用户组映射', 'userGroupMapping', NULL, '/api/v1/userGroup/mapping', '2024-08-14 20:47:28.030165', 'admin', NULL, NULL, 1, 'c0b4ee30-bf40-4299-9fab-ff32328b047a', 'f');

-- ----------------------------
-- Records of t_resource_group
-- ----------------------------
INSERT INTO "t_resource_group" ("resource_group_id", "resource_group_name", "resource_group_code", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('c0b4ee30-bf40-4299-9fab-ff32328b047a', '认证授权服务资源组', '52cb8d26-a352-4e5c-99a7-d52b8afff3b1', NULL, '2024-08-14 00:26:47.121378', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_role
-- ----------------------------
INSERT INTO "t_role" ("role_id", "role_name", "role_code", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('baec302c-39ac-4e51-9d28-fb8c9c43caa3', '认证授权服务系统管理员', 'AUTH_SERVER_ADMIN', '', '2024-08-14 00:55:17.101532', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_role_mapping
-- ----------------------------
INSERT INTO "t_role_mapping" ("user_id", "role_id", "user_group_id") VALUES ('4a7eb192-b0e8-4678-bf81-bbbd70ba1880', 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL);

-- ----------------------------
-- Records of t_user
-- ----------------------------
INSERT INTO "t_user" ("user_id", "username", "password", "phone_number", "email_address", "enable_mfa", "mfa_secret", "need_change_pwd", "create_time", "create_by", "update_time", "update_by", "version", "deleted", "locked", "last_login_time", "last_login_ip", "last_login_device_type", "last_login_device_os", "login_failed_cnt", "mfa_device_bind", "console_access") VALUES ('4a7eb192-b0e8-4678-bf81-bbbd70ba1880', 'admin', '$2a$10$j5n/3CfXdla8AY90GYTszO1G97zD8CT5BVdT5rMvA3byqi0x1X3.O', NULL, NULL, 'f', NULL, 't', '2024-08-14 00:28:08.771614', NULL, NULL, NULL, 1, 'f', 'f', NULL, NULL, NULL, NULL, NULL, NULL, 't');

-- ----------------------------
-- Records of t_user_attr
-- ----------------------------
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('d019fb4e-8acd-4411-9061-9d8aee961703', 'userId', '用户ID', 'STRING', 'f', 'f', NULL, 240);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('3cb048e2-5896-46aa-96e6-fa975b4780f5', 'username', '用户名', 'STRING', 'f', 't', 0, 180);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('ff289375-461b-4e6f-8e16-9187d7e44a14', 'emailAddress', '邮箱地址', 'STRING', 'f', 't', 1, 180);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('3845b5d4-36a0-45bb-854e-6d79593aefd4', 'phoneNumber', '手机号码', 'STRING', 'f', 't', 2, 180);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('08604464-2832-44e3-8bd4-d8dbda7db9d7', 'createTime', '创建时间', 'STRING', 'f', 't', 3, 180);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('edf63d7e-fa66-4483-b38b-1cd7200b05ec', 'country', '国家', 'STRING', 't', 'f', NULL, 180);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('1c092d28-81fb-4e97-92ef-93d446c826c6', 'birthDate', '出生日期', 'DATETIME', 't', 'f', NULL, 180);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('37479a6e-e777-4114-9f81-c8e31f0ce49b', 'address', '住址', 'STRING', 't', 'f', NULL, 180);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('6a5a3759-3fb3-47c3-bec6-f14d32e170c2', 'locked', '禁用账号', 'BOOLEAN', 'f', 'f', NULL, 120);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('0965fca9-d005-4cd8-8a77-531d01b8fc05', 'enableMfa', '开启 MFA', 'BOOLEAN', 'f', 'f', NULL, 120);
INSERT INTO "t_user_attr" ("attr_id", "attr_key", "attr_name", "attr_data_type", "ext_attr_flg", "user_lst_display", "display_seq", "display_width") VALUES ('47c3b7fb-fbce-4410-aa1e-3b1353468d49', 'consoleAccess', '允许控制台访问', 'BOOLEAN', 'f', 'f', NULL, 120);


-- ----------------------------
-- Records of t_authorize
-- ----------------------------
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'c4395ddb-0ffc-4926-b695-7f0e7c923a31', '2b184cee-8898-4ea1-a595-0c005b1885a4', '2024-08-14 20:56:33.076127');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'a61250fc-2137-47ee-af75-89e818284a16', 'e513cd18-f8c2-4c21-bbe7-9d932840aa67', '2024-08-14 20:56:41.642992');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'a61250fc-2137-47ee-af75-89e818284a16', '8a0611e7-5a87-4345-a5b0-7536e78b8ef7', '2024-08-14 20:56:41.642992');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'a4864949-8375-44bb-8c89-162a90dae9fa', '10f9534d-b21e-478b-a28a-65035025e821', '2024-08-14 20:56:57.899431');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '1382716a-f327-41bf-89c6-dd096b35c636', '0bfbf39d-3197-4159-896b-395b6e917560', '2024-08-14 20:57:01.847329');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '09ab38a0-2cf7-4394-b69f-5039826b6f6e', '89415226-7575-499e-a979-4e44e3bf781a', '2024-08-14 20:57:05.851822');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '7001267e-b258-4c78-81e9-b029c5e35169', '34688794-4ea8-4709-bec7-ea75e80f3c07', '2024-08-14 20:57:09.381468');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '7819e0f4-3ebd-4ce7-ad8d-304e485eff03', 'e467a23c-a2b5-47d1-ba01-ccd1c1e76041', '2024-08-14 20:57:13.072297');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '47c8a3e9-f822-40a0-9681-d9e19bda7039', '53dd9c9f-f860-49eb-9968-5b272d342a4c', '2024-08-14 20:57:17.47886');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'cb6b3296-1885-43e8-aef9-3e3cbcf47dc2', '197f5d4e-8f9f-4ba6-b3ee-1ecdadb3d169', '2024-08-14 20:57:21.618259');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '0e27fb8b-d3fa-43e8-acfa-4f744e1e9a38', '28c82de2-6994-46ce-8b45-25b91ceacb0f', '2024-08-14 20:57:31.764626');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '4d936c24-e90b-4fc7-b201-8830a46496dc', '8cf27fa6-3ef9-4cfc-b1ee-90f15f307f65', '2024-08-14 20:57:36.485573');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '6fc2e212-ce2d-4248-80ce-2850902a8e63', 'ca5758ea-e0d7-4471-82ac-e6741010fecd', '2024-08-14 20:57:42.182381');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '261ece17-1a3a-442c-b37a-b8bf0b849591', '20955bbe-adc3-4b12-a852-2167af90913c', '2024-08-14 20:57:47.871169');
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'b4c95f7f-77ea-432a-8d33-02073dfa14d4', 'dc1d6643-af7d-4ffe-be0d-d8929cf3f20c', '2024-08-14 20:57:53.456764');
