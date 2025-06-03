import apiRequest from "@/util/apiRequest";

/**
 * 获取启用的身份源
 *
 * @returns 启用的身份源
 */
export function getEnabledIdentitySource() {
  return apiRequest.get({
    url: "/identitySource/enabled",
  });
}

/**
 * 获取身份源提供商列表
 *
 * @param data 入参
 * @returns 身份源提供商列表
 */
export function getIdentitySourceProviderList(params: any) {
  return apiRequest.get({
    url: "/identitySource/provider/list",
    params,
  });
}

/**
 * 获取身份源提供商详情
 *
 * @param id 提供商ID
 * @returns 身份源提供商详情
 */
export function getIdentitySourceProviderDetail(id: string) {
  return apiRequest.get({
    url: `/identitySource/provider/${id}`,
  });
}

/**
 *  更新身份源提供商
 *
 * @param data 身份源提供商信息
 * @returns 响应结果
 */
export function updateIdentitySourceProvider(data: any) {
  return apiRequest.put({
    url: "/identitySource/provider",
    data,
  });
}

/**
 * 创建身份源提供商
 *
 * @param data 身份源提供商信息
 * @returns 响应结果
 */
export function createIdentitySourceProvider(data: any) {
  return apiRequest.post({
    url: "/identitySource/provider",
    data,
  });
}

/**
 * 获取关联的身份源
 *
 * @param providerId 身份源提供商ID
 * @returns 身份源列表
 */
export function getIdentitySourceRegistrations(providerId: string) {
  return apiRequest.get({
    url: `/identitySource/provider/${providerId}/registrations`,
  });
}

/**
 * 更新身份源
 *
 * @param data 身份源信息
 * @returns 响应结果
 */
export function updateIdentitySource(data: any) {
  return apiRequest.put({
    url: "/identitySource",
    data,
  });
}

/**
 * 获取身份源详情
 *
 * @param id 身份源ID
 * @returns 身份源详情
 */
export function getIdentitySourceDetail(id: string) {
  return apiRequest.get({
    url: `/identitySource/${id}`,
  });
}

/**
 * 创建身份源
 *
 * @param data 身份源信息
 * @returns 响应结果
 */
export function createIdentitySource(data: any) {
  return apiRequest.post({
    url: "/identitySource",
    data,
  });
}

/**
 * 获取用户绑定列表
 *
 * @param id 身份源ID
 * @param params 参数
 * @returns
 */
export function getUserBindingList(id: string, params: any) {
  return apiRequest.get({
    url: `/identitySource/${id}/userBindings`,
    params,
  });
}

/**
 * 获取身份源列表
 *
 * @param params 参数
 * @returns 身份源列表
 */
export function getIdentitySourceList(params: any) {
  return apiRequest.get({
    url: "/identitySource/list",
    params,
  });
}

/**
 * 删除身份源
 *
 * @param id 身份源ID
 * @returns 响应结果
 */
export function deleteIdentitySource(id: string) {
  return apiRequest.delete({
    url: `/identitySource/${id}`,
  });
}

/**
 * 删除身份源提供商
 * 
 * @param id 身份源提供商ID
 * @returns 响应结果
 */
export function deleteIdentitySourceProvider(id: string) {
  return apiRequest.delete({
    url: `/identitySource/provider/${id}`,
  });
}