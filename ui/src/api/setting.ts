import apiRequest from "@/util/apiRequest";

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