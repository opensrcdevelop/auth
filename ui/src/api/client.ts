import apiRequest from "@/util/apiRequest";
import request from "@/util/request";
import { getOAuthIssuer } from "@/util/tool";

/**
 * 获取客户端列表
 *
 * @param data 入参
 * @returns 客户端列表
 */
export function getClientList(data: any) {
  return apiRequest.get({
    url: "/client/list",
    params: data,
  });
}

/**
 * 获取客户端详情
 *
 * @param id 客户端ID
 * @returns 客户端详情
 */
export function getClientDetail(id: string) {
  return apiRequest.get({
    url: `/client/${id}`,
  });
}

/**
 * 获取 Oidc 端点信息
 */
export function getOidcEndpointInfo() {
  return request.get({
    url: `${getOAuthIssuer()}/.well-known/openid-configuration`,
  });
}

/**
 * 更新客户端信息
 *
 * @param data 客户端信息
 * @returns 调用结果
 */
export function updateClientDetail(data: any) {
  return apiRequest.put({
    url: "/client",
    data,
  });
}

/**
 * 刷新客户端密钥
 *
 * @param id 客户端 ID
 * @returns 调用结果
 */
export function updateClientSecret(id: string) {
  return apiRequest.put({
    url: `/client/secret/${id}`,
  });
}

/**
 * 创建客户端
 *
 * @param data 创建客户端表单
 * @returns 调用结果
 */
export function createClient(data: any) {
  return apiRequest.post({
    url: "/client",
    data,
  });
}

/**
 * 删除客户端
 * 
 * @param id 客户端 ID
 * @returns 调用结果
 */
export function deleteClient(id: string) {
  return apiRequest.delete({
    url: `/client/${id}`,
  });
}
