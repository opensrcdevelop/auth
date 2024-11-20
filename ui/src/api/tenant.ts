import apiRequest from "@/util/apiRequest";

/**
 * 获取租户列表
 *
 * @param params 请求参数
 * @returns 租户列表
 */
export function getTenantList(params: any) {
  return apiRequest.get({
    url: "/tenant/list",
    params,
  });
}

/**
 * 获取租户详情
 *
 * @param id 租户 ID
 * @returns 租户详情
 */
export function getTenantDetail(id: string) {
  return apiRequest.get({
    url: `/tenant/${id}`,
  });
}

/**
 * 更新租户信息
 *
 * @param data 租户信息
 * @returns 调用结果
 */
export function updateTenant(data: any) {
  return apiRequest.put({
    url: "/tenant",
    data,
  });
}

/**
 * 创建租户
 *
 * @param data 创建租户表单
 * @returns 调用结果
 */
export function createTenant(data: any) {
  return apiRequest.post({
    url: "/tenant",
    data,
  });
}

/**
 * 删除租户
 *
 * @param id 租户 ID
 * @returns 调用结果
 */
export function deleteTenant(id: string) {
  return apiRequest.delete({
    url: `/tenant/${id}`,
  });
}

/**
 * 检查租户是否存在
 *
 * @param code 租户标识
 * @returns 调用结果
 */
export function checkTenant(code: string) {
  return apiRequest.get({
    url: `/tenant/check/${code}`,
  });
}
