import apiRequest from "@/util/apiRequest";
import authRequest from "@/util/authRequest";
import { base64Str } from "@/util/tool";
/**
 * 提交登录表单
 *
 * @param data 登录表单数据
 * @returns 登录状态
 */
export function loginSubmit(data) {
    return authRequest.post({
        url: "/login",
        data: data,
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
    });
}
/**
 * Totp 动态安全码校验
 *
 * @param data 数据
 * @returns 校验状态
 */
export function totpValidSubmit(data) {
    return apiRequest.post({
        url: "/totp/check",
        data: data,
    });
}
/**
 * 提交邮箱登录表单
 *
 * @param data 邮箱登录表单数据
 * @returns 登录状态
 */
export function emailLoginSubmit(data) {
    return authRequest.post({
        url: "/login/email",
        data: data,
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
    });
}
/**
 * 发送邮箱验证码
 *
 * @param data 邮箱
 * @returns 发送状态
 */
export function sendEmailCodeSubmit(data) {
    return apiRequest.post({
        url: "/code/email/".concat(data),
    });
}
/**
 * 获取访问令牌
 *
 * @param data 参数
 * @returns 访问令牌
 */
export function getToken(data) {
    var headers = {
        "Content-Type": "application/x-www-form-urlencoded",
    };
    if (data.client_secret) {
        headers.Authorization = "Basic ".concat(base64Str("".concat(data.client_id, ":").concat(data.client_secret)));
        delete data.clicent_id;
        delete data.client_secret;
    }
    return authRequest.post({
        url: "/oauth2/token",
        data: data,
        headers: headers,
    });
}
/**
 * 变更密码
 *
 * @param data 变更密码表单
 * @returns 调用结果
 */
export function changePwd(data) {
    return apiRequest.post({
        url: "/user/me/password/change",
        data: data,
    });
}
/**
 * 检查验证码
 *
 * @param data 检查验证码表单
 * @returns 调用结果
 */
export function checkCode(data) {
    return apiRequest.post({
        url: "/code/check",
        data: data,
    });
}
/**
 * 重置密码
 *
 * @param data 重置密码表单
 * @returns 调用结果
 */
export function resetPwd(data) {
    return apiRequest.post({
        url: "/user/me/password/reset",
        data: data,
    });
}
