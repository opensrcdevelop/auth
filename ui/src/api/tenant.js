import apiRequest from "@/util/apiRequest";
/**
 * 获取租户列表
 *
 * @param params 请求参数
 * @returns 租户列表
 */
export function getTenantList(params) {
    return apiRequest.get({
        url: "/tenant/list",
        params: params,
    });
}
/**
 * 获取租户详情
 *
 * @param id 租户 ID
 * @returns 租户详情
 */
export function getTenantDetail(id) {
    return apiRequest.get({
        url: "/tenant/".concat(id),
    });
}
/**
 * 更新租户信息
 *
 * @param data 租户信息
 * @returns 调用结果
 */
export function updateTenant(data) {
    return apiRequest.put({
        url: "/tenant",
        data: data,
    });
}
/**
 * 创建租户
 *
 * @param data 创建租户表单
 * @returns 调用结果
 */
export function createTenant(data) {
    return apiRequest.post({
        url: "/tenant",
        data: data,
    });
}
/**
 * 删除租户
 *
 * @param id 租户 ID
 * @returns 调用结果
 */
export function deleteTenant(id) {
    return apiRequest.delete({
        url: "/tenant/".concat(id),
    });
}
/**
 * 检查租户是否存在
 *
 * @param code 租户标识
 * @returns 调用结果
 */
export function checkTenant(code) {
    return apiRequest.get({
        url: "/tenant/check/".concat(code),
    });
}
