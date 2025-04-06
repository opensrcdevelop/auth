import apiRequest from "@/util/apiRequest";
import noneLoadingApiRequest from "@/util/noneApiLoadingRequest";

/**
 * 获取邮件模版列表
 *
 * @returns 邮件模版列表
 */
export function getMailTemplateList() {
  return apiRequest.get({
    url: "/setting/mailTemplate/list",
  });
}

/**
 * 获取邮件模版详情
 *
 * @param id 邮件模版ID
 * @returns 邮件模版详情
 */
export function getMailTemplateDetail(id: string) {
  return apiRequest.get({
    url: `setting/mailTemplate/${id}`,
  });
}

/**
 * 更新邮件模版
 *
 * @param data 请求
 * @returns 响应结果
 */
export function updateMailTemplate(data: any) {
  return apiRequest.put({
    url: "/setting/mailTemplate",
    data,
  });
}

/**
 * 获取邮件服务配置
 *
 * @returns 邮件服务配置
 */
export function getMailServiceConfig() {
  return apiRequest.get({
    url: "/setting/mailService",
  });
}

/**
 * 保存邮件服务配置
 *
 * @param data 邮件服务配置
 * @returns 响应结果
 */
export function saveMailServiceConfig(data: any) {
  return apiRequest.post({
    url: "/setting/mailService",
    data,
  });
}

/**
 * 获取邮件消息配置
 *
 * @returns 邮件消息配置
 */
export function getMailMessageConfig() {
  return apiRequest.get({
    url: "/setting/mailMessage",
  });
}

/**
 *  保存邮件消息配置
 *
 * @param data 邮件消息配置
 * @returns
 */
export function saveMailMessageConfig(data: any) {
  return apiRequest.post({
    url: "/setting/mailMessage",
    data,
  });
}

/**
 * 获取密码策略列表
 *
 * @returns 密码策略列表
 */
export function getPasswordPolicyList() {
  return apiRequest.get({
    url: "/setting/passwordPolicy/list",
  });
}

/**
 * 创建密码策略
 *
 * @param data 密码策略
 * @returns 响应结果
 */
export function createPasswordPolicy(data: any) {
  return apiRequest.post({
    url: "/setting/passwordPolicy",
    data,
  });
}

/**
 * 更新密码策略优先级（执行顺序）
 *
 * @param data
 * @returns
 */
export function updatePasswordPolicyPriority(data: any) {
  return apiRequest.put({
    url: "/setting/passwordPolicy/priority",
    data,
  });
}

/**
 * 更新密码策略
 *
 * @param data 请求
 * @returns 响应结果
 */
export function updatePasswordPolicy(data: any) {
  return apiRequest.put({
    url: "/setting/passwordPolicy",
    data,
  });
}

/**
 * 删除密码策略
 *
 * @param id 密码策略 ID
 * @returns 响应结果
 */
export function deletePasswordPolicy(id: string) {
  return apiRequest.delete({
    url: `/setting/passwordPolicy/${id}`,
  });
}

/**
 * 获取密码策略详情
 *
 * @param id 密码策略 ID
 * @returns 密码策略详情
 */
export function getPasswordPolicyDdetail(id: string) {
  return apiRequest.get({
    url: `/setting/passwordPolicy/${id}`,
  });
}

/**
 * 检查密码
 *
 * @param data 请求数据
 * @returns 响应结果
 */
export function checkPassword(data: any) {
  return noneLoadingApiRequest.post({
    url: "/setting/passwordPolicy/check",
    data,
  });
}

/**
 *  检查密码（不直接使用密码策略）
 *
 * @param data 请求数据
 * @returns 响应结果
 */
export function checkPasswordWithoutPolicy(data: any) {
  return noneLoadingApiRequest.post({
    url: "/setting/passwordPolicy/checkWithoutPolicy",
    data,
  });
}

/**
 * 获取密码策略提醒日志列表
 *
 * @param params 请求参数
 * @returns 密码策略提醒日志列表
 */
export function getUpdatePasswordRemindLogList(params: any) {
  return apiRequest.get({
    url: "/setting/passwordPolicy/remindLog/list",
    params,
  });
}
