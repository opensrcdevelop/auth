import {Notification} from "@arco-design/web-vue";
import CryptoJS from "crypto-js";
import {OAUTH_ISSUER, TENANT_CODE} from "./constants";

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
 * 解析 base64 字符串
 * 
 * @param str base64 字符串
 * @returns 原字符串
 */
export const decodeBase64Str = (str: string) => {
  return CryptoJS.enc.Base64.parse(str).toString(CryptoJS.enc.Utf8);
}

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
  const defaultHostname = new URL(import.meta.env.VITE_DEFAULT_CONSOLE_URL)
    .hostname;
  if (hostname === defaultHostname) {
    return "";
  }
  return hostname.split(".")[0];
};

/**
 * 获取 OAuth Issuer
 *
 * @returns
 */
export const getOAuthIssuer = () => {
  if (localStorage.getItem(OAUTH_ISSUER)) {
    return localStorage.getItem(OAUTH_ISSUER);
  }
  return import.meta.env.VITE_DEFAULT_OAUTH_ISSUER;
};

/**
 * 获取 Console Url
 *
 * @returns Console Url
 */
export const getConsoleUrl = () => {
  if (localStorage.getItem(TENANT_CODE)) {
    const tmpUrl = new URL(import.meta.env.VITE_DEFAULT_CONSOLE_URL);
    return `${tmpUrl.protocol}//${localStorage.getItem(TENANT_CODE)}.${
      tmpUrl.hostname
    }${tmpUrl.port ? `:${tmpUrl.port}` : ""}`;
  } else {
    return import.meta.env.VITE_DEFAULT_CONSOLE_URL;
  }
};

/**
 * 判断是否为租户
 * 
 * @returns 是否为租户
 */
export const isTenant = () => {
  return localStorage.getItem(TENANT_CODE) !== null;
}

/**
 * 复制文本至剪切板
 * 
 * @param text 文件
 * @returns 复制结果
 */
 export const copyToClipboard = async (text: string): Promise<boolean> => {
    if (navigator.clipboard && window.isSecureContext) {
      try {
        await navigator.clipboard.writeText(text);
        return true;
      } catch (err) {
        console.warn("Clipboard API 失败:", err);
      }
    }

    try {
      const textArea = document.createElement("textarea");
      textArea.value = text;

      textArea.style.position = "fixed";
      textArea.style.top = "0";
      textArea.style.left = "0";
      textArea.style.width = "2em";
      textArea.style.height = "2em";
      textArea.style.padding = "0";
      textArea.style.border = "none";
      textArea.style.outline = "none";
      textArea.style.boxShadow = "none";
      textArea.style.background = "transparent";
      textArea.style.opacity = "0";

      document.body.appendChild(textArea);
      textArea.select();
      textArea.setSelectionRange(0, 99999);

      const successful = document.execCommand("copy");
      document.body.removeChild(textArea);

      if (successful) {
        return true;
      }
    } catch (err) {
      console.warn("execCommand 方法失败:", err);
    }

    try {
      const selection = document.getSelection();
      const range = document.createRange();
      const div = document.createElement("div");
      div.textContent = text;

      document.body.appendChild(div);
      range.selectNodeContents(div);
      selection?.removeAllRanges();
      selection?.addRange(range);

      const successful = document.execCommand("copy");
      selection?.removeAllRanges();
      document.body.removeChild(div);

      if (successful) {
        return true;
      }
    } catch (err) {
      console.warn("Selection API 方法失败:", err);
    }
  };