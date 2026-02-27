import apiRequest from "@/util/apiRequest";
import noneLoadingApiRequest from "@/util/noneApiLoadingRequest";
/**
 * 取消授权
 *
 * @param permissionId 权限ID
 * @param principalId 用户 / 用户组 / 角色ID
 * @returns 调用结果
 */
export function cancelAuthorization(permissionId, principalId) {
    return apiRequest.delete({
        url: "/permission/authorize/".concat(permissionId, "/").concat(principalId),
    });
}
/**
 * 获取权限详情
 *
 * @param id 权限ID
 * @params 请求参数
 * @returns 权限详情
 */
export function getPermissionDetail(id, params) {
    return apiRequest.get({
        url: "/permission/".concat(id),
        params: params,
    });
}
/**
 * 创建权限
 *
 * @param data 创建权限表单
 * @returns 调用结果
 */
export function createPermission(data) {
    return apiRequest.post({
        url: "/permission",
        data: data,
    });
}
/**
 * 删除权限
 *
 * @param id 权限ID
 * @returns 调用结果
 */
export function deletePermission(id) {
    return apiRequest.delete({
        url: "/permission/".concat(id),
    });
}
/**
 * 更新权限
 *
 * @param data 更新权限表单
 * @returns 调用结果
 */
export function updatePermission(data) {
    return apiRequest.put({
        url: "/permission",
        data: data,
    });
}
/**
 * 获取权限表达式列表
 *
 * @param params 请求参数
 */
export function getPermissionExpList(params) {
    return apiRequest.get({
        url: "/permission/exp/list",
        params: params,
    });
}
/**
 * 获取权限表达式详情
 *
 * @param id 权限表达式ID
 * @returns 权限表达式详情
 */
export function getPermissionExpDetail(id) {
    return apiRequest.get({
        url: "/permission/exp/".concat(id),
    });
}
/**
 * 获取权限表达式关联的权限列表
 *
 * @param id 权限表达式ID
 * @returns 权限表达式关联的权限列表
 */
export function getPermissionExpPermissions(id) {
    return apiRequest.get({
        url: "/permission/exp/".concat(id, "/permissions"),
    });
}
/**
 * 删除授权条件
 *
 * @param data 请求
 * @returns 响应结果
 */
export function removeAuthorizeCondition(data) {
    return apiRequest.delete({
        url: "/permission/authorize/cond",
        data: data,
    });
}
/**
 * 添加授权条件
 *
 * @param data 请求
 * @returns 响应结果
 */
export function addAuthorizeCondition(data) {
    return apiRequest.post({
        url: "/permission/authorize/cond",
        data: data,
    });
}
/**
 * 更新权限表达式
 *
 * @param data 更新权限表达式表单
 * @returns 调用结果
 */
export function updatePermissionExp(data) {
    return apiRequest.put({
        url: "/permission/exp",
        data: data,
    });
}
/**
 * 删除权限表达式
 *
 * @param id 权限表达式ID
 * @returns 调用结果
 */
export function deletePermissionExp(id) {
    return apiRequest.delete({
        url: "/permission/exp/".concat(id),
    });
}
/**
 * 创建权限表达式
 *
 * @param data 创建权限表达式表单
 * @returns 调用结果
 */
export function createPermissionExp(data) {
    return apiRequest.post({
        url: "/permission/exp",
        data: data,
    });
}
/**
 * 授权
 *
 * @param data 授权表单
 * @returns 调用结果
 */
export function authorize(data) {
    return apiRequest.post({
        url: "/permission/authorize",
        data: data,
    });
}
/**
 * 调试权限表达式
 *
 * @param data 调试权限表达式表单
 * @returns 调用结果
 */
export function debugPermissionExp(data) {
    return noneLoadingApiRequest.post({
        url: "/permission/exp/debug",
        data: data,
    });
}
/**
 * 更新授权优先级
 *
 * @param id 授权ID
 * @param priority 优先级
 * @returns 调用结果
 */
export function updateAuthorizePriority(id, priority) {
    return apiRequest.put({
        url: "/permission/authorize/".concat(id, "/").concat(priority)
    });
}
/**
 * 创建权限表达式模板
 *
 * @param data 创建权限表达式模板表单
 * @returns 调用结果
 */
export function createPermissionExpTemplate(data) {
    return apiRequest.post({
        url: "/permission/exp/template",
        data: data,
    });
}
/**
 * 获取权限表达式模板列表
 *
 * @param params 请求参数
 * @returns 权限表达式模板列表
 */
export function getPermissionExpTemplateList(params) {
    return apiRequest.get({
        url: "/permission/exp/template/list",
        params: params,
    });
}
/**
 * 获取权限表达式模板详情
 *
 * @param id 权限表达式模板ID
 * @returns 权限表达式模板详情
 */
export function getPermissionExpTemplateDetail(id) {
    return apiRequest.get({
        url: "/permission/exp/template/".concat(id),
    });
}
/**
 * 更新权限表达式模板
 *
 * @param data 更新权限表达式模板表单
 * @returns 调用结果
 */
export function updatePermissionExpTemplate(data) {
    return apiRequest.put({
        url: "/permission/exp/template",
        data: data,
    });
}
/**
 * 删除权限表达式模板
 *
 * @param id 模板ID
 * @returns 调用结果
 */
export function deletePermissionExpTemplate(id) {
    return apiRequest.delete({
        url: "/permission/exp/template/".concat(id),
    });
}
/**
 * 获取权限表达式模板关联的权限表达式列表
 *
 * @param id 模版ID
 * @returns 权限表达式模板关联的权限表达式列表
 */
export function getPermissionExpTemplateExpList(id) {
    return apiRequest.get({
        url: "/permission/exp/template/".concat(id, "/exps"),
    });
}
/**
 * 获取权限表达式模板参数配置
 *
 * @param id 模板ID
 * @returns 模板参数配置
 */
export function getPremissionExpTemplateParamConfigs(id) {
    return apiRequest.get({
        url: "/permission/exp/template/".concat(id, "/params"),
    });
}
