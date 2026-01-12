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
export function getIdentitySourceProviderList(params) {
    return apiRequest.get({
        url: "/identitySource/provider/list",
        params: params,
    });
}
/**
 * 获取身份源提供商详情
 *
 * @param id 提供商ID
 * @returns 身份源提供商详情
 */
export function getIdentitySourceProviderDetail(id) {
    return apiRequest.get({
        url: "/identitySource/provider/".concat(id),
    });
}
/**
 *  更新身份源提供商
 *
 * @param data 身份源提供商信息
 * @returns 响应结果
 */
export function updateIdentitySourceProvider(data) {
    return apiRequest.put({
        url: "/identitySource/provider",
        data: data,
    });
}
/**
 * 创建身份源提供商
 *
 * @param data 身份源提供商信息
 * @returns 响应结果
 */
export function createIdentitySourceProvider(data) {
    return apiRequest.post({
        url: "/identitySource/provider",
        data: data,
    });
}
/**
 * 获取关联的身份源
 *
 * @param providerId 身份源提供商ID
 * @returns 身份源列表
 */
export function getIdentitySourceRegistrations(providerId) {
    return apiRequest.get({
        url: "/identitySource/provider/".concat(providerId, "/registrations"),
    });
}
/**
 * 更新身份源
 *
 * @param data 身份源信息
 * @returns 响应结果
 */
export function updateIdentitySource(data) {
    return apiRequest.put({
        url: "/identitySource",
        data: data,
    });
}
/**
 * 获取身份源详情
 *
 * @param id 身份源ID
 * @returns 身份源详情
 */
export function getIdentitySourceDetail(id) {
    return apiRequest.get({
        url: "/identitySource/".concat(id),
    });
}
/**
 * 创建身份源
 *
 * @param data 身份源信息
 * @returns 响应结果
 */
export function createIdentitySource(data) {
    return apiRequest.post({
        url: "/identitySource",
        data: data,
    });
}
/**
 * 获取用户绑定列表
 *
 * @param id 身份源ID
 * @param params 参数
 * @returns
 */
export function getUserBindingList(id, params) {
    return apiRequest.get({
        url: "/identitySource/".concat(id, "/userBindings"),
        params: params,
    });
}
/**
 * 获取身份源列表
 *
 * @param params 参数
 * @returns 身份源列表
 */
export function getIdentitySourceList(params) {
    return apiRequest.get({
        url: "/identitySource/list",
        params: params,
    });
}
/**
 * 删除身份源
 *
 * @param id 身份源ID
 * @returns 响应结果
 */
export function deleteIdentitySource(id) {
    return apiRequest.delete({
        url: "/identitySource/".concat(id),
    });
}
/**
 * 删除身份源提供商
 *
 * @param id 身份源提供商ID
 * @returns 响应结果
 */
export function deleteIdentitySourceProvider(id) {
    return apiRequest.delete({
        url: "/identitySource/provider/".concat(id),
    });
}
/**
 * 获取绑定的身份源
 *
 * @returns 绑定的身份源
 */
export function getBoundIdentitySource() {
    return apiRequest.get({
        url: "/identitySource/bound"
    });
}
/**
 * 绑定当前用户
 *
 * @param code 身份源标识
 * @returns 响应结果
 */
export function bindUser(code) {
    return apiRequest.get({
        url: "/identitySource/bind/".concat(code)
    });
}
/**
 * 解绑当前用户
 *
 * @param id 身份源ID
 * @returns 响应结果
 */
export function unbindUser(id) {
    return apiRequest.delete({
        url: "/identitySource/".concat(id, "/unbind")
    });
}
