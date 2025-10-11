/**
 * 变更：
 *      1. 添加表及字段注释
 *      2. 表【t_resource、t_permission、t_authorize】添加删除数据
 *
 */

COMMENT ON COLUMN "t_audit_log"."id" IS '主键';
COMMENT ON COLUMN "t_audit_log"."audit_id" IS '审计ID';
COMMENT ON COLUMN "t_audit_log"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_audit_log"."audit_type" IS '审计类型';
COMMENT ON COLUMN "t_audit_log"."operation_type" IS '操作类型';
COMMENT ON COLUMN "t_audit_log"."resource_id" IS '资源ID';
COMMENT ON COLUMN "t_audit_log"."operation_result" IS '操作结果';
COMMENT ON COLUMN "t_audit_log"."operation_time" IS '操作时间';
COMMENT ON COLUMN "t_audit_log"."operation_detail" IS '操作详情';
COMMENT ON COLUMN "t_audit_log"."ip" IS 'IP';
COMMENT ON COLUMN "t_audit_log"."ip_region" IS 'IP地区';
COMMENT ON COLUMN "t_audit_log"."device_type" IS '设备类型';
COMMENT ON COLUMN "t_audit_log"."os_type" IS '操作系统类型';
COMMENT ON COLUMN "t_audit_log"."browser_type" IS '浏览器类型';
COMMENT ON COLUMN "t_audit_log"."extra_info" IS '额外信息';
COMMENT ON COLUMN "t_audit_log"."request_id" IS '请求ID';
COMMENT ON TABLE "t_audit_log" IS '审计日志表';

COMMENT ON COLUMN "t_authorization"."id" IS '主键';
COMMENT ON COLUMN "t_authorization"."registered_client_id" IS '客户端ID';
COMMENT ON COLUMN "t_authorization"."principal_name" IS '用户名';
COMMENT ON COLUMN "t_authorization"."authorization_grant_type" IS '授权类型';
COMMENT ON COLUMN "t_authorization"."authorized_scopes" IS '已授权 scope';
COMMENT ON COLUMN "t_authorization"."attributes" IS '属性';
COMMENT ON COLUMN "t_authorization"."state" IS '状态';
COMMENT ON COLUMN "t_authorization"."authorization_code_value" IS '授权码';
COMMENT ON COLUMN "t_authorization"."authorization_code_issued_at" IS '授权码颁发时间';
COMMENT ON COLUMN "t_authorization"."authorization_code_expires_at" IS '授权码过期时间';
COMMENT ON COLUMN "t_authorization"."authorization_code_metadata" IS '授权码元数据';
COMMENT ON COLUMN "t_authorization"."access_token_value" IS '访问令牌';
COMMENT ON COLUMN "t_authorization"."access_token_issued_at" IS '访问令牌颁发时间';
COMMENT ON COLUMN "t_authorization"."access_token_expires_at" IS '访问令牌过期时间';
COMMENT ON COLUMN "t_authorization"."access_token_metadata" IS '访问令牌元数据';
COMMENT ON COLUMN "t_authorization"."access_token_type" IS '访问令牌类型';
COMMENT ON COLUMN "t_authorization"."access_token_scopes" IS '访问令牌授权的 scope';
COMMENT ON COLUMN "t_authorization"."refresh_token_value" IS '刷新令牌';
COMMENT ON COLUMN "t_authorization"."refresh_token_issued_at" IS '刷新令牌颁发时间';
COMMENT ON COLUMN "t_authorization"."refresh_token_expires_at" IS '刷新令牌过期时间';
COMMENT ON COLUMN "t_authorization"."refresh_token_metadata" IS '刷新令牌元数据';
COMMENT ON COLUMN "t_authorization"."oidc_id_token_value" IS 'OIDC令牌';
COMMENT ON COLUMN "t_authorization"."oidc_id_token_issued_at" IS 'OIDC令牌颁发时间';
COMMENT ON COLUMN "t_authorization"."oidc_id_token_expires_at" IS 'OIDC令牌过期时间';
COMMENT ON COLUMN "t_authorization"."oidc_id_token_metadata" IS 'OIDC令牌元数据';
COMMENT ON COLUMN "t_authorization"."oidc_id_token_claims" IS 'OIDC令牌Claim';
COMMENT ON COLUMN "t_authorization"."user_code_value" IS '用户码';
COMMENT ON COLUMN "t_authorization"."user_code_issued_at" IS '用户码颁发时间';
COMMENT ON COLUMN "t_authorization"."user_code_expires_at" IS '用户码过期时间';
COMMENT ON COLUMN "t_authorization"."user_code_metadata" IS '用户码元数据';
COMMENT ON COLUMN "t_authorization"."device_code_value" IS '设备码';
COMMENT ON COLUMN "t_authorization"."device_code_issued_at" IS '设备码颁发时间';
COMMENT ON COLUMN "t_authorization"."device_code_expires_at" IS '设备码过期时间';
COMMENT ON COLUMN "t_authorization"."device_code_metadata" IS '设备码元数据';
COMMENT ON COLUMN "t_authorization"."login_id" IS '登录ID';
COMMENT ON TABLE "t_authorization" IS '授权信息记录表，存储授权码、访问令牌等';

COMMENT ON COLUMN "t_authorize"."id" IS '主键';
COMMENT ON COLUMN "t_authorize"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_authorize"."role_id" IS '角色ID';
COMMENT ON COLUMN "t_authorize"."user_group_id" IS '用户组ID';
COMMENT ON COLUMN "t_authorize"."permission_id" IS '权限ID';
COMMENT ON COLUMN "t_authorize"."authorize_id" IS '授权ID';
COMMENT ON COLUMN "t_authorize"."authorize_time" IS '授权时间';
COMMENT ON COLUMN "t_authorize"."priority" IS '优先级';
COMMENT ON TABLE "t_authorize" IS '授权关系表，记录用户、角色、用户组与权限的关联';

COMMENT ON COLUMN "t_chat_answer"."id" IS '主键';
COMMENT ON COLUMN "t_chat_answer"."answer_id" IS '回答ID';
COMMENT ON COLUMN "t_chat_answer"."data_source_id" IS '数据源ID';
COMMENT ON COLUMN "t_chat_answer"."chat_id" IS '对话ID';
COMMENT ON COLUMN "t_chat_answer"."question_id" IS '问题ID';
COMMENT ON COLUMN "t_chat_answer"."question" IS '问题';
COMMENT ON COLUMN "t_chat_answer"."model_provider_id" IS '模型提供商ID';
COMMENT ON COLUMN "t_chat_answer"."model" IS '模型';
COMMENT ON COLUMN "t_chat_answer"."chart_config" IS '图表配置JSON';
COMMENT ON COLUMN "t_chat_answer"."sql" IS '生成的SQL语句';
COMMENT ON COLUMN "t_chat_answer"."report_type" IS '报告类型';
COMMENT ON COLUMN "t_chat_answer"."report" IS '报告';
COMMENT ON COLUMN "t_chat_answer"."answer" IS '回答';
COMMENT ON COLUMN "t_chat_answer"."feedback" IS '反馈';
COMMENT ON COLUMN "t_chat_answer"."req_tokens" IS '请求消耗的令牌数';
COMMENT ON COLUMN "t_chat_answer"."rep_tokens" IS '响应消耗的令牌数';
COMMENT ON TABLE "t_chat_answer" IS '对话回答记录表';

COMMENT ON COLUMN "t_chat_history"."id" IS '主键';
COMMENT ON COLUMN "t_chat_history"."chat_id" IS '对话ID';
COMMENT ON COLUMN "t_chat_history"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_chat_history"."title" IS '标题';
COMMENT ON COLUMN "t_chat_history"."description" IS '描述';
COMMENT ON COLUMN "t_chat_history"."start_time" IS '开始时间';
COMMENT ON COLUMN "t_chat_history"."end_time" IS '结束时间';
COMMENT ON COLUMN "t_chat_history"."data_source_id" IS '数据源ID';
COMMENT ON TABLE "t_chat_history" IS '对话历史记录表';

COMMENT ON COLUMN "t_chat_message_history"."id" IS '主键';
COMMENT ON COLUMN "t_chat_message_history"."message_id" IS '消息ID';
COMMENT ON COLUMN "t_chat_message_history"."role" IS '消息角色';
COMMENT ON COLUMN "t_chat_message_history"."type" IS '消息类型';
COMMENT ON COLUMN "t_chat_message_history"."content" IS '消息类型';
COMMENT ON COLUMN "t_chat_message_history"."question_id" IS '问题ID';
COMMENT ON COLUMN "t_chat_message_history"."chat_id" IS '对话ID';
COMMENT ON COLUMN "t_chat_message_history"."rewritten_question" IS '重写后的问题';
COMMENT ON COLUMN "t_chat_message_history"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_chat_message_history"."create_time" IS '创建时间';
COMMENT ON COLUMN "t_chat_message_history"."time" IS '发送时间';
COMMENT ON COLUMN "t_chat_message_history"."answer_id" IS '回答ID';
COMMENT ON TABLE "t_chat_message_history" IS '对话消息历史记录表';


COMMENT ON COLUMN "t_client"."id" IS '主键';
COMMENT ON COLUMN "t_client"."client_id" IS '客户端ID';
COMMENT ON COLUMN "t_client"."client_id_issued_at" IS '客户端ID颁发时间';
COMMENT ON COLUMN "t_client"."client_secret" IS '客户端密钥';
COMMENT ON COLUMN "t_client"."client_secret_expires_at" IS '客户端密钥过期时间';
COMMENT ON COLUMN "t_client"."client_name" IS '客户端名称';
COMMENT ON COLUMN "t_client"."client_authentication_methods" IS '客户端认证方法';
COMMENT ON COLUMN "t_client"."authorization_grant_types" IS '支持的授权类型';
COMMENT ON COLUMN "t_client"."redirect_uris" IS '重定向URL';
COMMENT ON COLUMN "t_client"."post_logout_redirect_uris" IS '登出后重定向URL';
COMMENT ON COLUMN "t_client"."scopes" IS '拥有的 scope';
COMMENT ON COLUMN "t_client"."client_settings" IS '客户端配置';
COMMENT ON COLUMN "t_client"."token_settings" IS '令牌配置';
COMMENT ON COLUMN "t_client"."description" IS '描述';
COMMENT ON TABLE "t_client" IS 'OAuth2 客户端表';

COMMENT ON COLUMN "t_data_source_conf"."id" IS '主键';
COMMENT ON COLUMN "t_data_source_conf"."data_source_id" IS '数据源ID';
COMMENT ON COLUMN "t_data_source_conf"."data_source_name" IS '数据源名称';
COMMENT ON COLUMN "t_data_source_conf"."data_source_type" IS '数据源类型';
COMMENT ON COLUMN "t_data_source_conf"."database" IS '数据库';
COMMENT ON COLUMN "t_data_source_conf"."host" IS '主机地址';
COMMENT ON COLUMN "t_data_source_conf"."port" IS '端口';
COMMENT ON COLUMN "t_data_source_conf"."username" IS '用户名';
COMMENT ON COLUMN "t_data_source_conf"."password" IS '密码';
COMMENT ON COLUMN "t_data_source_conf"."enabled" IS '是否启用';
COMMENT ON COLUMN "t_data_source_conf"."last_sync_table_time" IS '最后同步表的时间';
COMMENT ON COLUMN "t_data_source_conf"."sync_table_count" IS '同步表的次数';
COMMENT ON COLUMN "t_data_source_conf"."system_ds" IS '是否是系统数据源';
COMMENT ON COLUMN "t_data_source_conf"."description" IS '描述';
COMMENT ON COLUMN "t_data_source_conf"."schema" IS '模式';
COMMENT ON COLUMN "t_data_source_conf"."jdbc_params" IS 'JDBC连接参数';
COMMENT ON TABLE "t_data_source_conf" IS 'ChatBI 数据源配置表';

COMMENT ON COLUMN "t_dict"."id" IS '主键';
COMMENT ON COLUMN "t_dict"."dict_id" IS '字典ID';
COMMENT ON COLUMN "t_dict"."dict_code" IS '字典标识';
COMMENT ON COLUMN "t_dict"."dict_name" IS '字典名称';
COMMENT ON COLUMN "t_dict"."description" IS '描述';
COMMENT ON TABLE "t_dict" IS '数据字典表';

COMMENT ON COLUMN "t_dict_data"."id" IS '主键';
COMMENT ON COLUMN "t_dict_data"."data_id" IS '字典数据ID';
COMMENT ON COLUMN "t_dict_data"."dict_id" IS '字典ID';
COMMENT ON COLUMN "t_dict_data"."data_label" IS '字典数据标签';
COMMENT ON COLUMN "t_dict_data"."data_value" IS '字典数据值';
COMMENT ON COLUMN "t_dict_data"."enable" IS '是否启用';
COMMENT ON COLUMN "t_dict_data"."display_seq" IS '显示顺序';
COMMENT ON TABLE "t_dict_data" IS '字典数据表';

COMMENT ON COLUMN "t_identity_source_provider"."id" IS '主键';
COMMENT ON COLUMN "t_identity_source_provider"."provider_id" IS '提供商ID';
COMMENT ON COLUMN "t_identity_source_provider"."provider_name" IS '提供商名称';
COMMENT ON COLUMN "t_identity_source_provider"."provider_code" IS '提供商标识';
COMMENT ON COLUMN "t_identity_source_provider"."provider_desc" IS '提供商描述';
COMMENT ON COLUMN "t_identity_source_provider"."provider_logo" IS '提供商 Logo';
COMMENT ON COLUMN "t_identity_source_provider"."authorization_uri" IS '授权 URL';
COMMENT ON COLUMN "t_identity_source_provider"."token_uri" IS '获取令牌 URL';
COMMENT ON COLUMN "t_identity_source_provider"."user_info_uris" IS '获取用户信息 URL';
COMMENT ON COLUMN "t_identity_source_provider"."user_info_authentication_method" IS '获取用户信息认证方法';
COMMENT ON COLUMN "t_identity_source_provider"."username_attribute" IS '用户信息中用户名属性名称 ';
COMMENT ON COLUMN "t_identity_source_provider"."unique_id_attribute" IS '用户信息中用户ID名称 ';
COMMENT ON COLUMN "t_identity_source_provider"."user_match_attribute" IS '用户信息中与本系统配置的属性名称 ';
COMMENT ON COLUMN "t_identity_source_provider"."jwk_set_uri" IS 'JWK URL';
COMMENT ON COLUMN "t_identity_source_provider"."issuer_uri" IS 'ISSUSER URL';
COMMENT ON COLUMN "t_identity_source_provider"."meta_data" IS '元数据';
COMMENT ON COLUMN "t_identity_source_provider"."scopes" IS 'scope';
COMMENT ON COLUMN "t_identity_source_provider"."enable_custom_authz_req" IS '是否启用自定义授权请求';
COMMENT ON COLUMN "t_identity_source_provider"."authz_req_cfg" IS '自定义授权请求配置';
COMMENT ON COLUMN "t_identity_source_provider"."enable_custom_token_req" IS '是否启用自定义获取令牌请求';
COMMENT ON COLUMN "t_identity_source_provider"."token_req_cfg" IS '自定义获取令牌请求配置';
COMMENT ON COLUMN "t_identity_source_provider"."enable_custom_user_info_req" IS '是否启用自定义获取用户信息请求';
COMMENT ON COLUMN "t_identity_source_provider"."user_info_req_cfg" IS '自定义获取用户信息请求配置';
COMMENT ON TABLE "t_identity_source_provider" IS '身份源提供商表';

COMMENT ON COLUMN "t_identity_source_registration"."id" IS '主键';
COMMENT ON COLUMN "t_identity_source_registration"."registration_id" IS '身份源ID';
COMMENT ON COLUMN "t_identity_source_registration"."provider_id" IS '身份源提供商ID';
COMMENT ON COLUMN "t_identity_source_registration"."registration_name" IS '身份源名称';
COMMENT ON COLUMN "t_identity_source_registration"."registration_code" IS '身份源标识';
COMMENT ON COLUMN "t_identity_source_registration"."client_id" IS '客户端ID';
COMMENT ON COLUMN "t_identity_source_registration"."client_secret" IS '客户端密钥';
COMMENT ON COLUMN "t_identity_source_registration"."client_authentication_method" IS '客户端认证方式';
COMMENT ON COLUMN "t_identity_source_registration"."authorization_grant_type" IS '客户端授权类型';
COMMENT ON COLUMN "t_identity_source_registration"."additional_params" IS '客户端额外参数';
COMMENT ON COLUMN "t_identity_source_registration"."enabled" IS '是否启用';
COMMENT ON TABLE "t_identity_source_registration" IS '身份源注册表';

COMMENT ON COLUMN "t_login_log"."id" IS '主键';
COMMENT ON COLUMN "t_login_log"."login_id" IS '登录ID';
COMMENT ON COLUMN "t_login_log"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_login_log"."session_id" IS '会话ID';
COMMENT ON COLUMN "t_login_log"."client_id" IS '客户端ID';
COMMENT ON COLUMN "t_login_log"."login_ip" IS '登录IP';
COMMENT ON COLUMN "t_login_log"."device_type" IS '设备类型';
COMMENT ON COLUMN "t_login_log"."device_os" IS '设备OS';
COMMENT ON COLUMN "t_login_log"."browser_type" IS '浏览器类型';
COMMENT ON COLUMN "t_login_log"."login_time" IS '登录时间';
COMMENT ON COLUMN "t_login_log"."login_ip_region" IS '登录IP地区';
COMMENT ON TABLE "t_login_log" IS '登录日志表';

COMMENT ON COLUMN "t_mail_template"."id" IS '主键';
COMMENT ON COLUMN "t_mail_template"."template_id" IS '模板ID';
COMMENT ON COLUMN "t_mail_template"."template_code" IS '模板标识';
COMMENT ON COLUMN "t_mail_template"."template_name" IS '模板名称';
COMMENT ON COLUMN "t_mail_template"."template_content" IS '模板内容';
COMMENT ON COLUMN "t_mail_template"."template_parameters" IS '模板参数';
COMMENT ON COLUMN "t_mail_template"."subject" IS '主题';
COMMENT ON COLUMN "t_mail_template"."sender" IS '发送人';
COMMENT ON COLUMN "t_mail_template"."description" IS '描述';
COMMENT ON TABLE "t_mail_template" IS '邮件模板表';

COMMENT ON COLUMN "t_model_provider"."id" IS '主键';
COMMENT ON COLUMN "t_model_provider"."provider_id" IS '提供商ID';
COMMENT ON COLUMN "t_model_provider"."provider_name" IS '提供商名称';
COMMENT ON COLUMN "t_model_provider"."provider_type" IS '提供商类型';
COMMENT ON COLUMN "t_model_provider"."base_url" IS 'API基础 URL';
COMMENT ON COLUMN "t_model_provider"."api_key" IS 'API密钥';
COMMENT ON COLUMN "t_model_provider"."optional_models" IS '可选模型';
COMMENT ON COLUMN "t_model_provider"."default_model" IS '默认模型';
COMMENT ON COLUMN "t_model_provider"."temperature" IS '温度参数';
COMMENT ON COLUMN "t_model_provider"."max_tokens" IS '最大回答 token 数';
COMMENT ON COLUMN "t_model_provider"."enabled" IS '是否启用';
COMMENT ON TABLE "t_model_provider" IS 'ChatBI 模型提供商表';

COMMENT ON COLUMN "t_multi_chat_memory"."id" IS '主键';
COMMENT ON COLUMN "t_multi_chat_memory"."chat_id" IS '对话ID';
COMMENT ON COLUMN "t_multi_chat_memory"."prompt_template" IS '提示词模板名称';
COMMENT ON COLUMN "t_multi_chat_memory"."content" IS '消息内容';
COMMENT ON COLUMN "t_multi_chat_memory"."type" IS '消息类型';
COMMENT ON COLUMN "t_multi_chat_memory"."create_time" IS '创建时间';
COMMENT ON TABLE "t_multi_chat_memory" IS '多提示词模板对话记忆表';

COMMENT ON COLUMN "t_obj_change_log"."id" IS '主键';
COMMENT ON COLUMN "t_obj_change_log"."audit_id" IS '审计日志ID';
COMMENT ON COLUMN "t_obj_change_log"."java_type" IS 'java对象类型';
COMMENT ON COLUMN "t_obj_change_log"."obj_id" IS 'java对象ID';
COMMENT ON COLUMN "t_obj_change_log"."before" IS '变更前的 java 对象';
COMMENT ON COLUMN "t_obj_change_log"."after" IS '变更后的 java 对象';
COMMENT ON TABLE "t_obj_change_log" IS '对象变更日志表';

COMMENT ON COLUMN "t_password_policy"."id" IS '主键';
COMMENT ON COLUMN "t_password_policy"."policy_id" IS '策略ID';
COMMENT ON COLUMN "t_password_policy"."policy_name" IS '策略名称';
COMMENT ON COLUMN "t_password_policy"."description" IS '策略描述';
COMMENT ON COLUMN "t_password_policy"."password_strength" IS '密码强度';
COMMENT ON COLUMN "t_password_policy"."custom_strength_config" IS '自定义密码强度配置';
COMMENT ON COLUMN "t_password_policy"."enabled" IS '是否启用';
COMMENT ON COLUMN "t_password_policy"."priority" IS '优先级';
COMMENT ON COLUMN "t_password_policy"."enable_password_detection" IS '是否开启用户登录密码强度检查';
COMMENT ON COLUMN "t_password_policy"."enable_force_change_password" IS '是否开启强制修改密码';
COMMENT ON COLUMN "t_password_policy"."forced_cycle" IS '强制修改密码周期';
COMMENT ON COLUMN "t_password_policy"."forced_cycle_unit" IS '强制修改密码周期单位';
COMMENT ON COLUMN "t_password_policy"."remind_cycle" IS '密码到期提醒周期';
COMMENT ON COLUMN "t_password_policy"."remind_cycle_unit" IS '密码到期提醒周期单位';
COMMENT ON TABLE "t_password_policy" IS '密码策略配置表';

COMMENT ON COLUMN "t_password_policy_mapping"."policy_id" IS '密码策略ID';
COMMENT ON COLUMN "t_password_policy_mapping"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_password_policy_mapping"."user_group_id" IS '用户组ID';
COMMENT ON TABLE "t_password_policy_mapping" IS '密码策略与用户、用户组映射表';

COMMENT ON COLUMN "t_permission"."id" IS '主键';
COMMENT ON COLUMN "t_permission"."permission_id" IS '权限ID';
COMMENT ON COLUMN "t_permission"."permission_name" IS '权限名';
COMMENT ON COLUMN "t_permission"."permission_code" IS '权限标识';
COMMENT ON COLUMN "t_permission"."description" IS '描述';
COMMENT ON COLUMN "t_permission"."resource_id" IS '资源ID';
COMMENT ON TABLE "t_permission" IS '权限定义表';

COMMENT ON COLUMN "t_permission_exp"."id" IS '主键';
COMMENT ON COLUMN "t_permission_exp"."expression_id" IS '表达式ID';
COMMENT ON COLUMN "t_permission_exp"."expression_name" IS '表达式名称';
COMMENT ON COLUMN "t_permission_exp"."expression" IS '表达式';
COMMENT ON COLUMN "t_permission_exp"."description" IS '描述';
COMMENT ON COLUMN "t_permission_exp"."template_id" IS '模板ID';
COMMENT ON COLUMN "t_permission_exp"."template_params" IS '模板参数';
COMMENT ON TABLE "t_permission_exp" IS '权限表达式表';

COMMENT ON COLUMN "t_permission_exp_template"."id" IS '主键';
COMMENT ON COLUMN "t_permission_exp_template"."template_id" IS '模板ID';
COMMENT ON COLUMN "t_permission_exp_template"."template_name" IS '模板名称';
COMMENT ON COLUMN "t_permission_exp_template"."expression" IS '表达式';
COMMENT ON COLUMN "t_permission_exp_template"."template_param_configs" IS '模板参数配置';
COMMENT ON COLUMN "t_permission_exp_template"."description" IS '描述';
COMMENT ON TABLE "t_permission_exp_template" IS '权限表达式模板表';

COMMENT ON COLUMN "t_resource"."id" IS '主键';
COMMENT ON COLUMN "t_resource"."resource_id" IS '资源ID';
COMMENT ON COLUMN "t_resource"."resource_name" IS '资源名称';
COMMENT ON COLUMN "t_resource"."resource_code" IS '资源标识';
COMMENT ON COLUMN "t_resource"."resource_group_id" IS '资源组ID';
COMMENT ON COLUMN "t_resource"."api_identifier" IS 'API URL 标识';
COMMENT ON COLUMN "t_resource"."description" IS '描述';
COMMENT ON TABLE "t_resource" IS '系统资源表';

COMMENT ON COLUMN "t_resource_group"."id" IS '主键';
COMMENT ON COLUMN "t_resource_group"."resource_group_id" IS '资源组ID';
COMMENT ON COLUMN "t_resource_group"."resource_group_name" IS '资源组名';
COMMENT ON COLUMN "t_resource_group"."resource_group_code" IS '资源组标识';
COMMENT ON COLUMN "t_resource_group"."description" IS '描述';
COMMENT ON TABLE "t_resource_group" IS '系统资源分组表';

COMMENT ON COLUMN "t_role"."id" IS '主键';
COMMENT ON COLUMN "t_role"."role_id" IS '角色ID';
COMMENT ON COLUMN "t_role"."role_name" IS '角色名称';
COMMENT ON COLUMN "t_role"."role_code" IS '角色标识';
COMMENT ON COLUMN "t_role"."description" IS '描述';
COMMENT ON TABLE "t_role" IS '角色定义表';

COMMENT ON COLUMN "t_role_mapping"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_role_mapping"."role_id" IS '角色ID';
COMMENT ON COLUMN "t_role_mapping"."user_group_id" IS '用户表';
COMMENT ON TABLE "t_role_mapping" IS '角色与用户、用户组映射表';

COMMENT ON COLUMN "t_sys_setting"."key" IS '系统配置键';
COMMENT ON COLUMN "t_sys_setting"."value" IS '系统配置值';
COMMENT ON COLUMN "t_sys_setting"."description" IS '系统配置描述';
COMMENT ON TABLE "t_sys_setting" IS '系统配置表';

COMMENT ON COLUMN "t_table"."id" IS '主键';
COMMENT ON COLUMN "t_table"."data_source_id" IS '数据源ID';
COMMENT ON COLUMN "t_table"."table_id" IS '表ID';
COMMENT ON COLUMN "t_table"."table_name" IS '表名称';
COMMENT ON COLUMN "t_table"."remark" IS '表注释';
COMMENT ON COLUMN "t_table"."to_use" IS '是否使用';
COMMENT ON COLUMN "t_table"."additional_info" IS '补充信息';
COMMENT ON TABLE "t_table" IS 'ChatBI 数据源表信息记录表';

COMMENT ON COLUMN "t_table_field"."id" IS '主键';
COMMENT ON COLUMN "t_table_field"."table_id" IS '表ID';
COMMENT ON COLUMN "t_table_field"."field_id" IS '表字段ID';
COMMENT ON COLUMN "t_table_field"."field_name" IS '表字段名称';
COMMENT ON COLUMN "t_table_field"."field_type" IS '表字段类型';
COMMENT ON COLUMN "t_table_field"."remark" IS '表字段注释';
COMMENT ON COLUMN "t_table_field"."to_use" IS '使用使用';
COMMENT ON COLUMN "t_table_field"."additional_info" IS '补充信息';
COMMENT ON TABLE "t_table_field" IS 'ChatBI 数据源表字段信息记录表';

COMMENT ON COLUMN "t_tenant"."id" IS '主键';
COMMENT ON COLUMN "t_tenant"."tenant_id" IS '租户ID';
COMMENT ON COLUMN "t_tenant"."tenant_name" IS '租户名称';
COMMENT ON COLUMN "t_tenant"."tenant_code" IS '租户标识';
COMMENT ON COLUMN "t_tenant"."description" IS '描述';
COMMENT ON COLUMN "t_tenant"."enabled" IS '是否启用';
COMMENT ON TABLE "t_tenant" IS '租户信息表';

COMMENT ON COLUMN "t_third_account"."id" IS '主键';
COMMENT ON COLUMN "t_third_account"."user_id" IS '本系统用户ID';
COMMENT ON COLUMN "t_third_account"."registration_id" IS '身份源ID';
COMMENT ON COLUMN "t_third_account"."unique_id" IS '唯一标识';
COMMENT ON COLUMN "t_third_account"."username" IS '用户名';
COMMENT ON COLUMN "t_third_account"."details" IS '用户详细信息';
COMMENT ON TABLE "t_third_account" IS '第三方账号关联表';

COMMENT ON COLUMN "t_update_password_remind_log"."id" IS '主键';
COMMENT ON COLUMN "t_update_password_remind_log"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_update_password_remind_log"."policy_id" IS '策略ID';
COMMENT ON COLUMN "t_update_password_remind_log"."remind_method" IS '提醒方式';
COMMENT ON COLUMN "t_update_password_remind_log"."remind_time" IS '提醒时间';
COMMENT ON COLUMN "t_update_password_remind_log"."success" IS '是否成功';
COMMENT ON TABLE "t_update_password_remind_log" IS '密码更新提醒发送日志表';

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
COMMENT ON COLUMN "t_user"."login_failed_cnt" IS '登录失败次数';
COMMENT ON COLUMN "t_user"."last_update_password_time" IS '最后修改密码时间';
COMMENT ON TABLE "t_user" IS '用户基本信息表';

COMMENT ON COLUMN "t_user_attr"."id" IS '主键';
COMMENT ON COLUMN "t_user_attr"."attr_id" IS '属性ID';
COMMENT ON COLUMN "t_user_attr"."attr_key" IS '属性键';
COMMENT ON COLUMN "t_user_attr"."attr_name" IS '属性名';
COMMENT ON COLUMN "t_user_attr"."attr_data_type" IS '属性数据类型';
COMMENT ON COLUMN "t_user_attr"."ext_attr_flg" IS '扩展属性标记';
COMMENT ON COLUMN "t_user_attr"."user_lst_display" IS '是否在用户列表显示';
COMMENT ON COLUMN "t_user_attr"."display_seq" IS '显示顺序';
COMMENT ON COLUMN "t_user_attr"."display_width" IS '显示宽度';
COMMENT ON COLUMN "t_user_attr"."user_visible" IS '用户可见';
COMMENT ON COLUMN "t_user_attr"."user_editable" IS '用户可编辑';
COMMENT ON COLUMN "t_user_attr"."dict_id" IS '字典ID';
COMMENT ON TABLE "t_user_attr" IS '用户属性定义表';

COMMENT ON COLUMN "t_user_attr_mapping"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_user_attr_mapping"."attr_id" IS '属性ID';
COMMENT ON COLUMN "t_user_attr_mapping"."attr_value" IS '属性值';
COMMENT ON TABLE "t_user_attr_mapping" IS '用户属性值映射表';

COMMENT ON COLUMN "t_user_group"."id" IS '主键';
COMMENT ON COLUMN "t_user_group"."user_group_id" IS '用户组ID';
COMMENT ON COLUMN "t_user_group"."user_group_name" IS '用户组名称';
COMMENT ON COLUMN "t_user_group"."user_group_code" IS '用户组标识';
COMMENT ON COLUMN "t_user_group"."description" IS '描述';
COMMENT ON TABLE "t_user_group" IS '用户组定义表';

COMMENT ON COLUMN "t_user_group_mapping"."user_id" IS '用户ID';
COMMENT ON COLUMN "t_user_group_mapping"."user_group_id" IS '用户组ID';
COMMENT ON TABLE "t_user_group_mapping" IS '用户与用户组映射关系表';


-- ----------------------------
-- 更新资源标识添加新资源
-- ----------------------------
UPDATE "t_resource" SET "resource_name" = 'ChatBI 问数-数据源' WHERE "resource_id" = '0199c427-9df0-7fbe-bfe4-de1a8cbebbfa';

DELETE FROM "t_resource" WHERE "resource_id" = '0199c923-b73d-73db-a654-76ddd12dd408';
DELETE FROM "t_resource" WHERE "resource_id" = '0199c926-f7e6-79d2-a864-f234ce930b07';
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c923-b73d-73db-a654-76ddd12dd408', 'ChatBI 问数-数据源', 'chatBIDataSource', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/chatbi/dataSourceConf', NULL, '2025-10-09 21:23:05.151199', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_resource" ("resource_id", "resource_name", "resource_code", "resource_group_id", "api_identifier", "description", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199c926-f7e6-79d2-a864-f234ce930b07', 'ChatBI 问数-模型提供商', 'chatBIModelProvider', 'c0b4ee30-bf40-4299-9fab-ff32328b047a', '/api/v1/chatbi/modelProvider', NULL, '2025-10-09 21:26:38.310711', 'admin', NULL, NULL, 1, 'f');


-- ----------------------------
-- 删除和添加权限
-- ----------------------------
DELETE FROM "t_permission" WHERE "permission_id" = '0199c429-d571-7c28-9961-ac7d853e7fc6';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42a-5cfd-72b0-b5aa-0062ec9102d3';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42b-0725-7c49-ab44-65c1ac59ac0d';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42b-8225-78c4-b5db-25083f979997';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42b-ed2f-7b02-8c97-614814bdcc6e';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42c-30e8-76d0-bc74-05bc2af9fd47';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42c-aa87-768f-b63e-86ba0d40fb06';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42c-fc63-7c45-860b-d637c85b0a2d';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42d-3108-7bca-8b17-09942f9b4656';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42e-621a-7fa5-b527-cf4570c50c54';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42e-cc01-75a1-a1a9-d15861aec84b';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42f-326b-7e28-aa55-c69c308a635f';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c42f-88c4-7122-a289-cb8c869548de';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c430-1534-7ac0-a832-3e167a1b2383';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c430-6bf1-7cf1-9d40-f369739ec8bf';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c430-b7ac-7b81-bbff-a8f0df4d67fc';
DELETE FROM "t_permission" WHERE "permission_id" = '0199c43d-d736-7ff3-b43c-88ccf7a45ffd';


DELETE FROM "t_permission" WHERE "permission_id" IN (
    '0199cea1-55dd-705a-86b7-3ea3e1731ac2',
    '0199cea4-a985-790a-9f11-883117f62b2c',
    '0199cea4-f087-78e1-a9f5-6c015e368dbf',
    '0199cea5-5a8a-7c4d-af82-7fa7b154993c',
    '0199cea5-98f8-7d68-bffa-9d96a826bf58',
    '0199cea5-d51a-7028-9ed8-e5651b4f8cd3',
    '0199cea6-179f-7fba-bb59-7c91e22d5a29',
    '0199cea6-7b90-7526-9646-5b9592f659f7',
    '0199cea6-af35-72ac-8990-c6c88fcb09c1',
    '0199cea6-db2e-729b-ad52-0a5120662dd9',
    '0199cea7-07b9-7902-8866-b9c574d4131c',
    '0199cea8-1308-7e51-bfb5-15d0eeb074cb',
    '0199cea8-855b-7174-b5e0-73a4544eb791',
    '0199cea8-b9f4-7c9f-ab21-2faa31fff9fd',
    '0199cea9-1420-7116-8553-fc739dc372fa',
    '0199cea9-4cad-741c-9b10-acdd8f184e69',
    '0199cea9-8f8d-7014-9960-bd57ab1647e5',
    '0199cea9-d9e9-7620-bad7-d1bbf131dd34'
);

INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea1-55dd-705a-86b7-3ea3e1731ac2', '所有权限', 'all', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 22:58:23.837273', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea4-a985-790a-9f11-883117f62b2c', '批量更新数据源表', 'batchUpdateTable', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:02:01.861659', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea4-f087-78e1-a9f5-6c015e368dbf', '批量更新数据源表字段', 'batchUpdateTableField', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:02:20.03964', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea5-5a8a-7c4d-af82-7fa7b154993c', '创建数据源配置', 'create', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:02:47.178857', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea5-98f8-7d68-bffa-9d96a826bf58', '删除数据源配置', 'delete', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:03:03.160927', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea5-d51a-7028-9ed8-e5651b4f8cd3', '获取数据源配置详情', 'detail', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:03:18.554105', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea6-179f-7fba-bb59-7c91e22d5a29', '获取数据源配置列表', 'list', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:03:35.584075', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea6-7b90-7526-9646-5b9592f659f7', '获取数据源表列表', 'tableList', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:04:01.168433', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea6-af35-72ac-8990-c6c88fcb09c1', '获取数据源表字段列表', 'tableFieldList', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:04:14.389253', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea6-db2e-729b-ad52-0a5120662dd9', '同步数据源表', 'syncTable', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:04:25.646247', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea7-07b9-7902-8866-b9c574d4131c', '测试数据源连接', 'testConn', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:04:37.049735', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea8-1308-7e51-bfb5-15d0eeb074cb', '更新数据源配置', 'update', NULL, '0199c923-b73d-73db-a654-76ddd12dd408', '2025-10-10 23:05:45.480972', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea8-855b-7174-b5e0-73a4544eb791', '所有权限', 'all', NULL, '0199c926-f7e6-79d2-a864-f234ce930b07', '2025-10-10 23:06:14.747176', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea8-b9f4-7c9f-ab21-2faa31fff9fd', '创建模型提供商', 'create', NULL, '0199c926-f7e6-79d2-a864-f234ce930b07', '2025-10-10 23:06:28.212869', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea9-1420-7116-8553-fc739dc372fa', '删除模型提供商', 'delete', NULL, '0199c926-f7e6-79d2-a864-f234ce930b07', '2025-10-10 23:06:51.296306', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea9-4cad-741c-9b10-acdd8f184e69', '获取模型提供商详情', 'detail', NULL, '0199c926-f7e6-79d2-a864-f234ce930b07', '2025-10-10 23:07:05.773358', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea9-8f8d-7014-9960-bd57ab1647e5', '获取模型提供商列表', 'list', NULL, '0199c926-f7e6-79d2-a864-f234ce930b07', '2025-10-10 23:07:22.893102', 'admin', NULL, NULL, 1, 'f');
INSERT INTO "t_permission" ("permission_id", "permission_name", "permission_code", "description", "resource_id", "create_time", "create_by", "update_time", "update_by", "version", "deleted") VALUES ('0199cea9-d9e9-7620-bad7-d1bbf131dd34', '更新模型提供商', 'update', NULL, '0199c926-f7e6-79d2-a864-f234ce930b07', '2025-10-10 23:07:41.929505', 'admin', NULL, NULL, 1, 'f');


-- ----------------------------
-- 添加授权
-- ----------------------------
DELETE FROM "t_authorize" WHERE "authorize_id" IN (
    '0199ceaa-4c42-75ae-a305-4c546ed25cc3',
    '0199ceaa-5daa-7f0e-9105-704bae754a6c'
);

INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '0199cea1-55dd-705a-86b7-3ea3e1731ac2', '0199ceaa-4c42-75ae-a305-4c546ed25cc3', '2025-10-10 23:08:11.202158', 0);
INSERT INTO "t_authorize" ("user_id", "role_id", "user_group_id", "permission_id", "authorize_id", "authorize_time", "priority") VALUES (NULL, 'baec302c-39ac-4e51-9d28-fb8c9c43caa3', NULL, '0199cea8-855b-7174-b5e0-73a4544eb791', '0199ceaa-5daa-7f0e-9105-704bae754a6c', '2025-10-10 23:08:15.658934', 0);
