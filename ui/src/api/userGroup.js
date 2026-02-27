import apiRequest from "@/util/apiRequest";
/**
 * 获取用户组列表
 *
 * @param params 请求参数
 * @returns 用户组列表
 */
export function getUserGroupList(params) {
    return apiRequest.get({
        url: "/userGroup/list",
        params: params,
    });
}
/**
 * 删除用户组映射
 *
 * @param data 用户组映射
 * @returns 调用结果
 */
export function removeUserGroupMapping(data) {
    return apiRequest.delete({
        url: "/userGroup/mapping",
        data: data,
    });
}
/**
 * 添加用户组映射
 *
 * @param data 用户组映射
 * @returns 调用结果
 */
export function addUserGroupMapping(data) {
    return apiRequest.post({
        url: "/userGroup/mapping",
        data: data,
    });
}
/**
 * 获取用户组详情
 *
 * @param id 用户组ID
 * @returns 用户组详情
 */
export function getUserGroupDetail(id) {
    return apiRequest.get({
        url: "/userGroup/".concat(id),
    });
}
/**
 * 获取组内用户
 *
 * @param id 用户组 ID
 * @param params 请求参数
 * @returns 组内用户
 */
export function getGroupUsers(id, params) {
    return apiRequest.get({
        url: "/userGroup/".concat(id, "/users"),
        params: params,
    });
}
/**
 * 更新用户组信息
 *
 * @param data 用户组信息
 * @returns 调用结果
 */
export function updateUserGroup(data) {
    return apiRequest.put({
        url: "/userGroup",
        data: data,
    });
}
/**
 * 创建用户组
 *
 * @param data 创建用户组表单
 * @returns 调用结果
 */
export function createUserGroup(data) {
    return apiRequest.post({
        url: "/userGroup",
        data: data,
    });
}
/**
 * 删除用户组
 *
 * @param id 用户组 ID
 * @returns 调用结果
 */
export function deleteUserGroup(id) {
    return apiRequest.delete({
        url: "/userGroup/".concat(id),
    });
}
/**
 * 获取用户组权限
 *
 * @param id 用户组 ID
 * @param params 请求参数
 * @returns 用户组权限
 */
export function getUserGroupPermissions(id, params) {
    return apiRequest.get({
        url: "/userGroup/".concat(id, "/permissions"),
        params: params,
    });
}
