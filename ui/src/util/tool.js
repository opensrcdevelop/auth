var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g = Object.create((typeof Iterator === "function" ? Iterator : Object).prototype);
    return g.next = verb(0), g["throw"] = verb(1), g["return"] = verb(2), typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
import { Notification } from "@arco-design/web-vue";
import CryptoJS from "crypto-js";
import { OAUTH_ISSUER, TENANT_CODE } from "./constants";
/**
 * 获取 Query 参数
 *
 * @param name 参数名
 * @returns 参数值
 */
export var getQueryString = function (name) {
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
export var handleApiError = function (error, message) {
    if (error.status === 403) {
        Notification.warning("\u65E0\u6743\u9650\u6267\u884C\u64CD\u4F5C\u3010".concat(message, "\u3011"));
        return;
    }
    if (error.data) {
        Notification.warning(error.data.message);
    }
    else {
        Notification.error("\u6267\u884C\u64CD\u4F5C\u3010".concat(message, "\u3011\u5931\u8D25\uFF08 ").concat(error.message, "\uFF09"));
    }
};
/**
 * API 成功处理
 *
 * @param result API 返回结果
 * @param successHandler 处理响应结果
 */
export var handleApiSuccess = function (result, successHandler) {
    if (result.success) {
        if (result.data) {
            successHandler(result.data);
        }
        else {
            successHandler();
        }
    }
    else {
        Notification.warning(result.message);
    }
};
/**
 * 生成随机字符串
 *
 * @param length 字符串长度
 * @returns 随机字符串
 */
export var generateRandomString = function (length) {
    var text = "";
    var possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    for (var i = 0; i < length; i++) {
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
export var base64Str = function (str) {
    return CryptoJS.enc.Base64.stringify(CryptoJS.enc.Utf8.parse(str));
};
/**
 * 解析 base64 字符串
 *
 * @param str base64 字符串
 * @returns 原字符串
 */
export var decodeBase64Str = function (str) {
    return CryptoJS.enc.Base64.parse(str).toString(CryptoJS.enc.Utf8);
};
/**
 * codeVerifier 转 codeChallenge
 *
 * @param codeVerifier codeVerifier
 * @returns codeChallenge
 */
export var generateCodeChallenge = function (codeVerifier) {
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
export var getSubDomain = function () {
    var hostname = window.location.hostname;
    var defaultHostname = new URL(import.meta.env.VITE_DEFAULT_CONSOLE_URL)
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
export var getOAuthIssuer = function () {
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
export var getConsoleUrl = function () {
    if (localStorage.getItem(TENANT_CODE)) {
        var tmpUrl = new URL(import.meta.env.VITE_DEFAULT_CONSOLE_URL);
        return "".concat(tmpUrl.protocol, "//").concat(localStorage.getItem(TENANT_CODE), ".").concat(tmpUrl.hostname).concat(tmpUrl.port ? ":".concat(tmpUrl.port) : "");
    }
    else {
        return import.meta.env.VITE_DEFAULT_CONSOLE_URL;
    }
};
/**
 * 判断是否为租户
 *
 * @returns 是否为租户
 */
export var isTenant = function () {
    return localStorage.getItem(TENANT_CODE) !== null;
};
/**
 * 复制文本至剪切板
 *
 * @param text 文件
 * @returns 复制结果
 */
export var copyToClipboard = function (text) { return __awaiter(void 0, void 0, void 0, function () {
    var err_1, textArea, successful, selection, range, div, successful;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                if (!(navigator.clipboard && window.isSecureContext)) return [3 /*break*/, 4];
                _a.label = 1;
            case 1:
                _a.trys.push([1, 3, , 4]);
                return [4 /*yield*/, navigator.clipboard.writeText(text)];
            case 2:
                _a.sent();
                return [2 /*return*/, true];
            case 3:
                err_1 = _a.sent();
                console.warn("Clipboard API 失败:", err_1);
                return [3 /*break*/, 4];
            case 4:
                try {
                    textArea = document.createElement("textarea");
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
                    successful = document.execCommand("copy");
                    document.body.removeChild(textArea);
                    if (successful) {
                        return [2 /*return*/, true];
                    }
                }
                catch (err) {
                    console.warn("execCommand 方法失败:", err);
                }
                try {
                    selection = document.getSelection();
                    range = document.createRange();
                    div = document.createElement("div");
                    div.textContent = text;
                    document.body.appendChild(div);
                    range.selectNodeContents(div);
                    selection === null || selection === void 0 ? void 0 : selection.removeAllRanges();
                    selection === null || selection === void 0 ? void 0 : selection.addRange(range);
                    successful = document.execCommand("copy");
                    selection === null || selection === void 0 ? void 0 : selection.removeAllRanges();
                    document.body.removeChild(div);
                    if (successful) {
                        return [2 /*return*/, true];
                    }
                }
                catch (err) {
                    console.warn("Selection API 方法失败:", err);
                }
                return [2 /*return*/];
        }
    });
}); };
