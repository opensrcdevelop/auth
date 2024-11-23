import apiRequest from "@/util/apiRequest";

/**
 * 获取资源组列表
 *
 * @param params 请求参数
 * @returns 资源组列表
 */
export function getResourceGroupList(params: any) {
  return apiRequest.get({
    url: "/resourceGroup/list",
    params,
  });
}

/**
 * 获取组内资源
 *
 * @param id 资源组ID
 * @param params 请求参数
 * @returns
 */
export function getGroupResources(id: string, params: any) {
  return apiRequest.get({
    url: `/resourceGroup/${id}/resources`,
    params,
  });
}

/**
 * 获取资源组详情
 *
 * @param id 资源组ID
 * @returns 资源组详情
 */
export function getResourceGroupDetail(id: string) {
  return apiRequest.get({
    url: `/resourceGroup/${id}`,
  });
}

/**
 * 更新资源组
 *
 * @param data 更新资源组表单
 * @returns 调用结果
 */
export function updateResourceGroup(data: any) {
  return apiRequest.put({
    url: "/resourceGroup",
    data,
  });
}

/**
 * 删除资源组
 *
 * @param id 资源组ID
 * @returns 调用结果
 */
export function deleteResourceGroup(id: string) {
  return apiRequest.delete({
    url: `/resourceGroup/${id}`,
  });
}

/**
 * 创建资源组
 *
 * @param data 创建资源组表单
 * @returns 调用结果
 */
export function createResourceGroup(data: any) {
  return apiRequest.post({
    url: "/resourceGroup",
    data,
  });
}
