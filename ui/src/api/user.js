import apiRequest from "@/util/apiRequest";
/**
 * 获取所有用户属性
 *
 * @param params 请求参数
 * @returns 所有用户属性
 */
export function getUserAttrs(params) {
    return apiRequest.get({
        url: "/user/attr/list",
        params: params,
    });
}
/**
 * 获取用户列表
 *
 * @param params 参数
 * @param data 请求体
 * @returns 用户列表
 */
export function getUserList(params, data) {
    if (data === void 0) { data = []; }
    return apiRequest.post({
        url: "/user/list",
        params: params,
        data: data,
    });
}
/**
 * 更新用户属性
 *
 * @param data 请求体
 * @returns 调用结果
 */
export function updateUserAttr(data) {
    return apiRequest.put({
        url: "/user/attr",
        data: data,
    });
}
/**
 * 设置用户属性显示顺序
 *
 * @param data 请求体
 * @returns 调用结果
 */
export function setUserAttrDisplaySeq(data) {
    return apiRequest.post({
        url: "/user/attr/seq",
        data: data,
    });
}
/**
 * 根据用户名搜索用户
 *
 * @param username 用户名
 * @param params 参数
 * @returns 搜索结果
 */
export function searchUser(username, params) {
    return apiRequest.post({
        url: "/user/list",
        params: params,
        data: [
            {
                key: "username",
                dataType: "STRING",
                value: username,
                filterType: "LIKE",
                extFlg: false,
            },
        ],
    });
}
/**
 * 获取用户详情
 *
 * @param id 用户 ID
 * @returns 用户详情
 */
export function getUserDetail(id) {
    return apiRequest.get({
        url: "/user/".concat(id),
    });
}
/**
 * 更新用户信息
 *
 * @param data 用户信息
 * @returns 调用结果
 */
export function updateUser(data) {
    return apiRequest.put({
        url: "/user",
        data: data,
    });
}
/**
 * 创建用户
 *
 * @param data 创建用户表单
 * @returns 调用结果
 */
export function createUser(data) {
    return apiRequest.post({
        url: "/user",
        data: data,
    });
}
/**
 * 删除用户
 *
 * @param id 用户ID
 * @returns 调用结果
 */
export function removeUser(id) {
    return apiRequest.delete({
        url: "/user/".concat(id),
    });
}
/**
 * 创建用户属性
 *
 * @param data 创建用户属性表单
 * @returns 调用结果
 */
export function createUserAttr(data) {
    return apiRequest.post({
        url: "/user/attr",
        data: data,
    });
}
/**
 * 获取用户属性详情
 *
 * @param id 用户属性ID
 * @returns 用户属性详情
 */
export function getUserAttrDetail(id) {
    return apiRequest.get({
        url: "/user/attr/".concat(id),
    });
}
/**
 * 删除用户属性
 *
 * @param id 用户属性ID
 * @returns 调用结果
 */
export function deleteUserAttr(id) {
    return apiRequest.delete({
        url: "/user/attr/".concat(id),
    });
}
/**
 * 获取当前用户信息
 *
 * @returns 当前用户信息
 */
export function getCurrentUser() {
    return apiRequest.get({
        url: "/user",
    });
}
/**
 * 重新绑定 TOTP 设备
 *
 * @param id 用户 ID
 * @returns 调用结果
 */
export function rebindTotpDevice(id) {
    return apiRequest.put({
        url: "/user/".concat(id, "/mfa/totp"),
    });
}
/**
 * 清空授权的 Token
 *
 * @param id 用户 ID
 * @returns 调用结果
 */
export function clearAuthorizedTokens(id) {
    return apiRequest.delete({
        url: "/user/".concat(id, "/token"),
    });
}
/**
 * 获取用户中心可见的用户属性
 *
 * @returns 用户中心可见的用户属性
 */
export function getVisibleUserAttrs() {
    return apiRequest.get({
        url: "/user/attr/list/visible",
    });
}
/**
 * 更新个人用户信息
 *
 * @param data 个人用户信息
 * @returns 调用结果
 */
export function updateMyUserInfo(data) {
    return apiRequest.put({
        url: "/user/me",
        data: data,
    });
}
/**
 * 发送绑定邮箱验证码
 *
 * @param to 邮箱
 * @returns 调用结果
 */
export function sendBindEmailCode(to) {
    return apiRequest.post({
        url: "/code/email/bind/".concat(to),
    });
}
/**
 * 绑定邮箱
 *
 * @param data 请求
 * @returns 调用结果
 */
export function bindEmail(data) {
    return apiRequest.post({
        url: "/user/me/email/bind",
        data: data,
    });
}
/**
 * 解绑邮箱
 *
 * @param data 请求
 * @returns 调用结果
 */
export function unbindEmail(data) {
    return apiRequest.post({
        url: "/user/me/email/unbind",
        data: data,
    });
}
/**
 * 获取用户权限
 *
 * @param id 用户 ID
 * @param params 请求参数
 * @returns 用户权限
 */
export function getUserPermissions(id, params) {
    return apiRequest.get({
        url: "/user/".concat(id, "/permissions"),
        params: params,
    });
}
/**
 * 获取用户登录日志
 *
 * @param id 用户 ID
 * @param params 请求参数
 * @returns
 */
export function getUserLoginLogs(id, params) {
    return apiRequest.get({
        url: "/user/".concat(id, "/loginLogs"),
        params: params,
    });
}
/**
 * 清空登录 ID 关联的 Token
 *
 * @param id 登录 ID
 * @returns 调用结果
 */
export function clearAuthorizedTokensByLoginId(id) {
    return apiRequest.delete({
        url: "/user/login/".concat(id, "/token"),
    });
}
/**
 * 下载用户导入模版
 *
 * @returns 模版文件 Blob
 */
export function downloadUserTemplate() {
    return apiRequest
        .get({
        url: "/user/excel/template",
        responseType: "blob",
    })
        .then(function (res) { return res.data; });
}
/**
 * 导出用户数据
 *
 * @param filters 筛选条件
 * @param all 是否导出全部
 * @param userIds 用户ID列表（用于导出当前页）
 * @returns 导出文件 Blob
 */
export function exportUsers(filters, all, userIds) {
    if (all === void 0) { all = false; }
    var url = "/user/excel/export?all=".concat(all);
    if (userIds && userIds.length > 0) {
        url += "&userIds=".concat(userIds.join(","));
    }
    return apiRequest
        .post({
        url: url,
        data: filters,
        responseType: "blob",
    })
        .then(function (res) { return res.data; });
}
/**
 * 导入用户数据
 *
 * @param file Excel 文件
 * @returns 导入结果
 */
export function importUsers(file) {
    var formData = new FormData();
    formData.append("file", file);
    // http.ts 拦截器会自动处理 FormData 的 Content-Type
    return apiRequest.post({
        url: "/user/excel/import",
        data: formData,
    });
}
/**
 * 异步导出用户数据
 *
 * @param filters 筛选条件
 * @param all 是否导出全部
 * @param userIds 指定用户 ID 列表
 * @param username 当前登录用户名
 * @returns taskId
 */
export function exportUsersAsync(filters, all, userIds, username) {
    if (all === void 0) { all = false; }
    var url = "/user/excel/export?all=".concat(all);
    if (userIds && userIds.length > 0) {
        url += "&userIds=".concat(userIds.join(","));
    }
    if (username) {
        url += "&username=".concat(encodeURIComponent(username));
    }
    return apiRequest.post({
        url: url,
        data: filters,
    });
}
/**
 * 异步导入用户数据
 *
 * @param file Excel 文件
 * @param username 当前登录用户名
 * @returns taskId
 */
export function importUsersAsync(file, username) {
    var formData = new FormData();
    formData.append("file", file);
    var url = username ? "/user/excel/import?username=".concat(encodeURIComponent(username)) : "/user/excel/import";
    return apiRequest.post({
        url: url,
        data: formData,
    });
}
/**
 * 重新绑定 TOTP 设备
 *
 * @param id 用户 ID
 * @returns 调用结果
 */
export function clearPasskeyCredentials(id) {
    return apiRequest.delete({
        url: "/user/".concat(id, "/mfa/passkey"),
    });
}
