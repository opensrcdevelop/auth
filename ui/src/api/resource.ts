import apiRequest from "@/util/apiRequest";

/**
 * 获取资源列表
 *
 * @param params 请求参数
 * @returns 资源列表
 */
export function getResourceList(params: any) {
  return apiRequest.get({
    url: "/resource/list",
    params,
  });
}

/**
 * 获取资源详情
 *
 * @param id 资源ID
 * @returns 资源详情
 */
export function getResourceDetail(id: string) {
  return apiRequest.get({
    url: `/resource/${id}`,
  });
}

/**
 * 获取资源内权限
 *
 * @param id 资源ID
 * @param params 请求参数
 * @returns 资源内权限
 */
export function getResourcePermissions(id: string, params: any) {
  return apiRequest.get({
    url: `/resource/${id}/permissions`,
    params,
  });
}

/**
 * 更新资源
 *
 * @param data 更新资源表单
 * @returns 调用结果
 */
export function updateResource(data: any) {
  return apiRequest.put({
    url: "/resource",
    data,
  });
}

/**
 * 创建资源
 *
 * @param data 创建资源表单
 * @returns 调用结果
 */
export function createResource(data: any) {
  return apiRequest.post({
    url: "/resource",
    data,
  });
}

/**
 * 删除资源
 *
 * @param id 资源ID
 * @returns 调用结果
 */
export function deleteResource(id: string) {
  return apiRequest.delete({
    url: `/resource/${id}`,
  });
}
