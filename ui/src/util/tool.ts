import { Notification } from "@arco-design/web-vue";
import CryptoJS from "crypto-js";

/**
 * 获取 Query 参数
 *
 * @param name 参数名
 * @returns 参数值
 */
export const getQueryString = (name: string) => {
  var reg = new RegExp("(^|&)" + name + "=([^&]*)(&|$)", "i");
  var r = window.location.search.substr(1).match(reg);
  if (r != null) {
    return unescape(r[2]);
  }
  return null;
};

/**
 * API 异常处理
 *
 * @param error 错误
 * @param message 消息
 */
export const handleApiError = (error: any, message: string) => {
  if (error.status === 403) {
    Notification.warning(`无权限执行操作【${message}】`);
    return;
  }

  if (error.data) {
    Notification.warning(error.data.message);
  } else {
    Notification.error(`执行操作【${message}】失败（ ${error.message}）`);
  }
};

/**
 * API 成功处理
 *
 * @param result API 返回结果
 * @param successHandler 处理响应结果
 */
export const handleApiSuccess = (result: any, successHandler: Function) => {
  if (result.success) {
    if (result.data) {
      successHandler(result.data);
    } else {
      successHandler();
    }
  } else {
    Notification.warning(result.message);
  }
};

/**
 * 生成随机字符串
 *
 * @param length 字符串长度
 * @returns 随机字符串
 */
export const generateRandomString = (length: number) => {
  let text = "";
  const possible =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < length; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }
  return text;
};

/**
 * 转 base64 字符串
 *
 * @param str 源
 * @returns base64 字符串
 */
export const base64Str = (str: string) => {
  return CryptoJS.enc.Base64.stringify(CryptoJS.enc.Utf8.parse(str));
};

/**
 * codeVerifier 转 codeChallenge
 *
 * @param codeVerifier codeVerifier
 * @returns codeChallenge
 */
export const generateCodeChallenge = (codeVerifier: string) => {
  return CryptoJS.SHA256(codeVerifier)
    .toString(CryptoJS.enc.Base64)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=/g, "");
};

/**
 * 获取子域名（租户标识）
 * 
 * @returns 子域名（租户标识）
 */
export const getSubDomain = () => {
  const hostname = window.location.hostname;
  const defaultHostname = new URL(import.meta.env.VITE_DEFAULT_CONSOLE_URL).hostname;
  if (hostname === defaultHostname) {
    return "";
  }
  return hostname.split(".")[0];
}

/**
 * 获取 OAuth Issuer
 * 
 * @returns 
 */
export const getOAuthIssuer = () => {
  if (localStorage.getItem("OAuthIssuer")) {
    return localStorage.getItem("OAuthIssuer");
  }
  return import.meta.env.VITE_DEFAULT_OAUTH_ISSUER;
}

/**
 * 获取 Console Url
 * 
 * @returns Console Url
 */
export const getConsoleUrl = () => {
  if (localStorage.getItem("tenantCode")) {
    const tmpUrl = new URL(import.meta.env.VITE_DEFAULT_CONSOLE_URL)
    return `${tmpUrl.protocol}//${localStorage.getItem("tenantCode")}.${tmpUrl.hostname}${tmpUrl.port ? `:${tmpUrl.port}` : ''}`
  } else {
    return import.meta.env.VITE_DEFAULT_CONSOLE_URL;
  }
}