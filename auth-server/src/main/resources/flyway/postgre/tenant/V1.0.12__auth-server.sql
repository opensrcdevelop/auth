/**
 * 变更：
 *      1. 添加表【t_identity_source_provider、t_identity_source_registration、t_third_account】
 *      2. 表【t_resource、t_permission、t_authorize】添加数据
 *
 */


-- ----------------------------
-- Table structure for t_identity_source_provider
-- ----------------------------
DROP TABLE IF EXISTS "t_identity_source_provider";
DROP SEQUENCE IF EXISTS "t_identity_source_provider_id_seq";
CREATE SEQUENCE "t_identity_source_provider_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

CREATE TABLE "t_identity_source_provider" (
  "id" int8 NOT NULL DEFAULT nextval('t_identity_source_provider_id_seq'::regclass),
  "provider_id" varchar(50) NOT NULL,
  "provider_name" varchar(255) NOT NULL,
  "provider_code" varchar(255) NOT NULL,
  "provider_desc" varchar(500),
  "provider_logo" varchar(255),
  "authorization_uri" varchar(255),
  "token_uri" varchar(255),
  "user_info_uris" text,
  "user_info_authentication_method" varchar(255),
  "username_attribute" varchar(255),
  "user_match_attribute" varchar(255),
  "jwk_set_uri" varchar(255),
  "issuer_uri" varchar(255),
  "meta_data" varchar(255),
  "scopes" varchar(255),
  "enable_custom_authz_req" bool,
  "authz_req_cfg" text,
  "enable_custom_token_req" bool,
  "token_req_cfg" text,
  "enable_custom_user_info_req" bool,
  "user_info_req_cfg" text,
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Primary Key structure for table t_identity_source_provider
-- ----------------------------
ALTER TABLE "t_identity_source_provider" ADD CONSTRAINT "t_identity_source_provider_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Records of t_identity_source_provider
-- ----------------------------
INSERT INTO "t_identity_source_provider" ("provider_id", "provider_name", "provider_code", "provider_desc", "provider_logo", "authorization_uri", "token_uri", "user_info_uris", "user_info_authentication_method", "username_attribute", "user_match_attribute", "jwk_set_uri", "issuer_uri", "meta_data", "create_time", "create_by", "update_time", "version", "deleted", "update_by", "scopes", "enable_custom_authz_req", "authz_req_cfg", "enable_custom_token_req", "token_req_cfg", "enable_custom_user_info_req", "user_info_req_cfg") VALUES ('996e0416-10a6-4b7f-b383-f66a49ade946', 'Google', 'google', 'Google 是全球最大的搜索引擎公司。', 'https://files.authing.co/authing-console/social-connections/google2.svg', 'https://accounts.google.com/o/oauth2/v2/auth', 'https://www.googleapis.com/oauth2/v4/token', 'https://www.googleapis.com/oauth2/v3/userinfo', 'header', 'id', 'email', NULL, NULL, NULL, '2025-05-25 23:38:01.456551', 'admin', NULL, 1, 'f', NULL, 'profile,email', 'f', NULL, 'f', NULL, 'f', NULL);
INSERT INTO "t_identity_source_provider" ("provider_id", "provider_name", "provider_code", "provider_desc", "provider_logo", "authorization_uri", "token_uri", "user_info_uris", "user_info_authentication_method", "username_attribute", "user_match_attribute", "jwk_set_uri", "issuer_uri", "meta_data", "create_time", "create_by", "update_time", "version", "deleted", "update_by", "scopes", "enable_custom_authz_req", "authz_req_cfg", "enable_custom_token_req", "token_req_cfg", "enable_custom_user_info_req", "user_info_req_cfg") VALUES ('1a97e168-145b-4349-a886-321285c3471b', 'Gitee', 'gitee', 'Gitee 是基于 Git 的代码托管和研发协作平台。', 'https://files.authing.co/authing-console/social-connections/gitee2.svg', 'https://gitee.com/oauth/authorize', 'https://gitee.com/oauth/token', 'https://gitee.com/api/v5/user', 'header', 'id', 'email', NULL, NULL, NULL, '2025-05-25 23:38:01.456551', 'admin', NULL, 1, 'f', NULL, '', 'f', NULL, 'f', NULL, 'f', NULL);
INSERT INTO "t_identity_source_provider" ("provider_id", "provider_name", "provider_code", "provider_desc", "provider_logo", "authorization_uri", "token_uri", "user_info_uris", "user_info_authentication_method", "username_attribute", "user_match_attribute", "jwk_set_uri", "issuer_uri", "meta_data", "create_time", "create_by", "update_time", "version", "deleted", "update_by", "scopes", "enable_custom_authz_req", "authz_req_cfg", "enable_custom_token_req", "token_req_cfg", "enable_custom_user_info_req", "user_info_req_cfg") VALUES ('93c29df9-df46-48a1-8050-1f11dd5783f4', 'Facebook', 'facebook', 'Facebook 是世界排名领先的照片分享站点。', 'https://files.authing.co/authing-console/social-connections/facebook2.svg', 'https://www.facebook.com/v2.8/dialog/oauth', 'https://graph.facebook.com/v2.8/oauth/access_token', 'https://graph.facebook.com/me?fields=id', 'header', 'id', 'email', NULL, NULL, NULL, '2025-05-25 23:42:43.623468', 'admin', NULL, 1, 'f', NULL, 'public_profile,email', 'f', NULL, 'f', NULL, 'f', NULL);
INSERT INTO "t_identity_source_provider" ("provider_id", "provider_name", "provider_code", "provider_desc", "provider_logo", "authorization_uri", "token_uri", "user_info_uris", "user_info_authentication_method", "username_attribute", "user_match_attribute", "jwk_set_uri", "issuer_uri", "meta_data", "create_time", "create_by", "update_time", "version", "deleted", "update_by", "scopes", "enable_custom_authz_req", "authz_req_cfg", "enable_custom_token_req", "token_req_cfg", "enable_custom_user_info_req", "user_info_req_cfg") VALUES ('bdc1e79f-156b-4f6f-8df3-44afc0ef2648', 'Github', 'github', 'GitHub 是一个面向开源及私有软件项目的托管平台。', 'https://files.authing.co/authing-console/social-connections/gitHub2.svg', 'https://github.com/login/oauth/authorize', 'https://github.com/login/oauth/access_token', 'https://api.github.com/user,https://api.github.com/user/emails', 'header', 'id', 'email', NULL, NULL, NULL, '2025-05-25 23:42:43.623468', 'admin', NULL, 1, 'f', NULL, 'read:user,user:email', 'f', NULL, 'f', NULL, 'f', NULL);
INSERT INTO "t_identity_source_provider" ("provider_id", "provider_name", "provider_code", "provider_desc", "provider_logo", "authorization_uri", "token_uri", "user_info_uris", "user_info_authentication_method", "username_attribute", "user_match_attribute", "jwk_set_uri", "issuer_uri", "meta_data", "create_time", "create_by", "update_time", "version", "deleted", "update_by", "scopes", "enable_custom_authz_req", "authz_req_cfg", "enable_custom_token_req", "token_req_cfg", "enable_custom_user_info_req", "user_info_req_cfg") VALUES ('5e66239e-2938-4747-915d-82a02740da25', '飞书', 'feishu', '飞书是先进企业协作与管理平台。', 'https://files.authing.co/authing-console/social-connections/feishu3.svg', 'https://accounts.feishu.cn/open-apis/authen/v1/authorize', 'https://open.feishu.cn/open-apis/authen/v2/oauth/token', 'https://open.feishu.cn/open-apis/authen/v1/user_info', 'header', 'union_id', 'email', NULL, NULL, NULL, '2025-05-31 00:57:24.009417', 'admin', NULL, 1, 'f', NULL, 'contact:user.email:readonly,contact:user.base:readonly', 'f', NULL, 'f', NULL, 'f', NULL);
INSERT INTO "t_identity_source_provider" ("provider_id", "provider_name", "provider_code", "provider_desc", "provider_logo", "authorization_uri", "token_uri", "user_info_uris", "user_info_authentication_method", "username_attribute", "user_match_attribute", "jwk_set_uri", "issuer_uri", "meta_data", "create_time", "create_by", "update_time", "version", "deleted", "update_by", "scopes", "enable_custom_authz_req", "authz_req_cfg", "enable_custom_token_req", "token_req_cfg", "enable_custom_user_info_req", "user_info_req_cfg") VALUES ('91a62c80-c722-4c85-93a5-963f2a3d8fe1', '华为', 'huawei', '华为是全球领先的计算机设备商，支持通过 HUAWEI 账号进行授权登录。', 'https://files.authing.co/authing-console/social-connections/huawei.svg', 'https://oauth-login.cloud.huawei.com/oauth2/v3/authorize', 'https://oauth-login.cloud.huawei.com/oauth2/v3/token', 'https://account.cloud.huawei.com/rest.php?nsp_svc=GOpen.User.getInfo', 'form', 'unionID', 'email', NULL, NULL, NULL, '2025-05-29 00:19:27.388046', 'admin', NULL, 1, 'f', NULL, 'email,profile', 'f', NULL, 'f', NULL, 'f', NULL);
INSERT INTO "t_identity_source_provider" ("provider_id", "provider_name", "provider_code", "provider_desc", "provider_logo", "authorization_uri", "token_uri", "user_info_uris", "user_info_authentication_method", "username_attribute", "user_match_attribute", "jwk_set_uri", "issuer_uri", "meta_data", "create_time", "create_by", "update_time", "version", "deleted", "update_by", "scopes", "enable_custom_authz_req", "authz_req_cfg", "enable_custom_token_req", "token_req_cfg", "enable_custom_user_info_req", "user_info_req_cfg") VALUES ('40d1590d-f870-45bb-ac51-0c4977f3b953', '钉钉', 'dingding', '钉钉是阿里巴巴出品，专为全球企业组织打造的智能移动办公平台。', 'https://files.authing.co/authing-console/social-connections/dingding2.svg', 'https://login.dingtalk.com/oauth2/auth', 'https://api.dingtalk.com/v1.0/oauth2/userAccessToken', 'https://api.dingtalk.com/v1.0/contact/users/me', 'header', 'unionId', 'mobile', '', NULL, NULL, '2025-06-02 14:46:58.784075', 'admin', NULL, 1, 'f', NULL, 'openid,corpid', 't', '{
  "params" : {
    "redirect_uri" : "${redirect_uri}",
    "response_type" : "${response_type}",
    "client_id" : "${client_id}",
    "prompt" : "consent",
    "corpId" : "${corp_id}",
    "scope" : "${scope}",
    "state" : "${state}"
  }
}', 't', '{
  "method" : "POST",
  "body" : {
    "clientId" : "${client_id}",
    "clientSecret" : "${client_secret}",
    "grantType" : "${grant_type}",
    "code" : "${code}"
  },
  "headers" : {
    "Content-Type" : "application/json"
  },
  "accessTokenAttr" : "accessToken"
}', 't', '{
  "https://api.dingtalk.com/v1.0/contact/users/me" : {
    "method" : "GET",
    "headers" : {
      "x-acs-dingtalk-access-token" : "${access_token}"
    }
  }
}');

-- ----------------------------
-- Table structure for t_identity_source_registration
-- ----------------------------
DROP TABLE IF EXISTS "t_identity_source_registration";
DROP SEQUENCE IF EXISTS "t_identity_source_registration_id_seq";
CREATE SEQUENCE "t_identity_source_registration_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

CREATE TABLE "t_identity_source_registration" (
  "id" int8 NOT NULL DEFAULT nextval('t_identity_source_registration_id_seq'::regclass),
  "registration_id" varchar(50) NOT NULL,
  "provider_id" varchar(50) NOT NULL,
  "registration_name" varchar(255) NOT NULL,
  "registration_code" varchar(255) NOT NULL,
  "client_id" varchar(255),
  "client_secret" varchar(255),
  "client_authentication_method" varchar(255),
  "authorization_grant_type" varchar(255),
  "additional_params" text,
  "enabled" bool DEFAULT true,
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Primary Key structure for table t_identity_source_registration
-- ----------------------------
ALTER TABLE "t_identity_source_registration" ADD CONSTRAINT "t_identity_source_registration_pkey" PRIMARY KEY ("id");

-- ----------------------------
-- Table structure for t_third_account
-- ----------------------------
DROP TABLE IF EXISTS "t_third_account";
DROP SEQUENCE IF EXISTS "t_third_account_id_seq";
CREATE SEQUENCE "t_third_account_id_seq"
    INCREMENT 1
    MINVALUE  1
    MAXVALUE 9223372036854775807
    START 1
    CACHE 1;

CREATE TABLE "t_third_account" (
  "id" int8 NOT NULL DEFAULT nextval('t_third_account_id_seq'::regclass),
  "user_id" varchar(50) NOT NULL,
  "registration_id" varchar(50) NOT NULL,
  "unique_id" varchar(255) NOT NULL,
  "details" text,
  "create_time" timestamp(6),
  "create_by" varchar(255),
  "update_time" timestamp(6),
  "update_by" varchar(255),
  "version" int8 DEFAULT 1,
  "deleted" bool DEFAULT false
)
;

-- ----------------------------
-- Records of t_resource
-- ----------------------------
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('97e86c03-4cea-4893-b78a-86d3af30cf0b', '身份源', 'identitySource', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/identitySource', NULL, '2025-06-04 21:59:44.670523', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('d87ced98-de68-4242-a1c5-ac85ee5074d3', '身份源提供商', 'identitySourceProvider', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/identitySource/provider', NULL, '2025-06-04 22:00:22.17704', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_permission
-- ----------------------------
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('2076e778-3003-4c37-9488-5c149016d306', '获取身份源列表', 'list', NULL, '97e86c03-4cea-4893-b78a-86d3af30cf0b', '2025-06-04 22:01:36.268394', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('b720485d-3855-4523-bb75-b15e56231f56', '创建身份源', 'create', NULL, '97e86c03-4cea-4893-b78a-86d3af30cf0b', '2025-06-04 22:02:03.688901', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('439614ee-2e90-47cd-a25c-4cc7f18998c6', '更新身份源', 'update', NULL, '97e86c03-4cea-4893-b78a-86d3af30cf0b', '2025-06-04 22:02:20.423258', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('27859cf1-1d45-447b-b2d1-e39de70e159a', '删除身份源', 'delete', NULL, '97e86c03-4cea-4893-b78a-86d3af30cf0b', '2025-06-04 22:02:48.604055', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('85e035cf-f642-4663-af44-5369e4dd9ec8', '获取身份源关联的用户绑定列表', 'userBindings', NULL, '97e86c03-4cea-4893-b78a-86d3af30cf0b', '2025-06-04 22:03:52.513843', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('dd756eed-c272-4492-af0d-171082f76733', '获取身份源提供商列表', 'list', NULL, 'd87ced98-de68-4242-a1c5-ac85ee5074d3', '2025-06-04 22:04:28.418423', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('50f5cef0-6b2e-47f2-a466-86f5ef3903b9', '获取身份源提供商详情', 'detail', NULL, 'd87ced98-de68-4242-a1c5-ac85ee5074d3', '2025-06-04 22:04:36.961874', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('cd742ce4-c32e-42f6-83c7-9e65342d2812', '创建身份源提供商', 'create', NULL, 'd87ced98-de68-4242-a1c5-ac85ee5074d3', '2025-06-04 22:04:44.901232', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('bf9dca0a-86b2-4b07-896f-632d77259905', '更新身份源提供商', 'update', NULL, 'd87ced98-de68-4242-a1c5-ac85ee5074d3', '2025-06-04 22:04:52.312046', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('aa057de2-e0e2-4554-8c2b-6c70d59472f0', '删除身份源提供商', 'delete', NULL, 'd87ced98-de68-4242-a1c5-ac85ee5074d3', '2025-06-04 22:05:03.238951', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('f2d07602-6337-43f7-bed5-e413e8ffe5a3', '获取身份源提供商关联的身份源列表', 'registrations', NULL, 'd87ced98-de68-4242-a1c5-ac85ee5074d3', '2025-06-04 22:05:35.507717', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('fc7de197-3e11-4476-ad45-bdca6599ed34', '获取身份源详情', 'detail', NULL, '97e86c03-4cea-4893-b78a-86d3af30cf0b', '2025-06-04 22:06:02.610245', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('8d0c0c81-81ae-4668-961b-aedce34e18cd', '所有权限', 'all', NULL, '97e86c03-4cea-4893-b78a-86d3af30cf0b', '2025-06-04 22:06:32.891422', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('bc11be5e-8ae9-43b4-888b-ac27eba9b6a9', '所有权限', 'all', NULL, 'd87ced98-de68-4242-a1c5-ac85ee5074d3', '2025-06-04 22:06:47.7374', 'admin', NULL, NULL, 1, 'f');

-- ----------------------------
-- Records of t_authorize
-- ----------------------------
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '8d0c0c81-81ae-4668-961b-aedce34e18cd', '924e655c-ece7-46e1-a1e3-161a521b3f64', '2025-06-04 22:20:01.415517', 0);
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, 'bc11be5e-8ae9-43b4-888b-ac27eba9b6a9', 'fe6cfc55-3b16-4f1b-9664-0da8a938a798', '2025-06-04 22:20:07.473771', 0);
