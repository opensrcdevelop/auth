var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
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
import axios from "axios";
import pinia from "@/store";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import router from "@/router";
import { base64Str, decodeBase64Str, getOAuthIssuer } from "./tool";
import { Notification } from "@arco-design/web-vue";
import { AUTH_TOKENS, REDIRECT_PATH, REDIRECT_QUERY, REFRESH_TOKEN, } from "./constants";
var globalVariables = useGlobalVariablesStore(pinia);
var Request = /** @class */ (function () {
    function Request(config, prefix, showGlobalLoading) {
        if (prefix === void 0) { prefix = ""; }
        if (showGlobalLoading === void 0) { showGlobalLoading = true; }
        var _this = this;
        // 基础配置
        this.baseConfig = {};
        // 前缀
        this.prefix = "";
        // 是否开启全局 Loading
        this.showGlobalLoading = true;
        this.instance = axios.create(Object.assign(this.baseConfig, config));
        this.prefix = prefix;
        this.showGlobalLoading = showGlobalLoading;
        this.instance.interceptors.request.use(function (config) {
            var _a;
            if (_this.showGlobalLoading) {
                globalVariables.apiLoading = true;
            }
            config.baseURL = prefix
                ? "".concat(getOAuthIssuer()).concat(prefix)
                : getOAuthIssuer();
            var authTokens = localStorage.getItem(AUTH_TOKENS);
            if (authTokens) {
                var token = JSON.parse(decodeBase64Str(authTokens));
                if (token &&
                    config.url !== "/oauth2/token" &&
                    !config.url.startsWith("/tenant/check/") &&
                    !config.url.startsWith("/captcha") &&
                    !config.url.startsWith("/identitySource/enabled")) {
                    config.headers.Authorization = "".concat(token.token_type, " ").concat(token.access_token);
                }
            }
            // 对于 FormData，不设置 Content-Type，让浏览器自动设置（包含 boundary）
            if (config.data instanceof FormData) {
                (_a = config.headers) === null || _a === void 0 ? true : delete _a["Content-Type"];
            }
            return config;
        }, function (err) {
            return Promise.reject(err);
        });
        this.instance.interceptors.response.use(function (res) {
            if (_this.showGlobalLoading) {
                globalVariables.apiLoading = false;
            }
            // 文件下载时直接返回原始响应
            if (res.config.responseType === "blob") {
                return res;
            }
            if (res.data) {
                return res.data;
            }
            return res;
        }, function (err) {
            if (_this.showGlobalLoading) {
                globalVariables.apiLoading = false;
            }
            if (err.response) {
                var messageText = "";
                switch (err.response.status) {
                    case 400:
                        messageText = "请求错误(400)";
                        break;
                    case 401:
                        messageText = "未授权，请重新登录(401)";
                        if (err.config.url !== "/oauth2/token") {
                            return refreshingToken(_this.instance, err.config);
                        }
                        break;
                    case 403:
                        messageText = "拒绝访问(403)";
                        break;
                    case 404:
                        messageText = "请求路径出错(404)";
                        break;
                    case 408:
                        messageText = "请求超时(408)";
                        break;
                    case 500:
                        messageText = "服务器错误(500)";
                        break;
                    case 501:
                        messageText = "服务未实现(501)";
                        break;
                    case 502:
                        messageText = "网络错误(502)";
                        break;
                    case 503:
                        messageText = "服务不可用(503)";
                        break;
                    case 504:
                        messageText = "网络超时(504)";
                        break;
                    case 505:
                        messageText = "HTTP版本不受支持(505)";
                        break;
                    default:
                        messageText = "\u8FDE\u63A5\u51FA\u9519(".concat(err.response.status, ")!");
                }
                err.response.statusText = messageText;
                return Promise.reject(err.response);
            }
            return Promise.reject(err);
        });
    }
    // 定义请求方法
    Request.prototype.request = function (config) {
        return this.instance.request(config);
    };
    Request.prototype.get = function (config) {
        return this.request(__assign(__assign({}, config), { method: "GET" }));
    };
    Request.prototype.post = function (config) {
        return this.request(__assign(__assign({}, config), { method: "POST" }));
    };
    Request.prototype.delete = function (config) {
        return this.request(__assign(__assign({}, config), { method: "DELETE" }));
    };
    Request.prototype.put = function (config) {
        return this.request(__assign(__assign({}, config), { method: "PUT" }));
    };
    Request.prototype.patch = function (config) {
        return this.request(__assign(__assign({}, config), { method: "PATCH" }));
    };
    return Request;
}());
export { Request };
// 是否正在刷新标记
var isRefreshing = false;
// 重试队列
var retryRequests = [];
/**
 * 刷新 token
 */
function refreshingToken(axios, requestConfig) {
    return __awaiter(this, void 0, void 0, function () {
        var authTokens, authTokensJson, res_1, err_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    if (!!isRefreshing) return [3 /*break*/, 7];
                    authTokens = localStorage.getItem(AUTH_TOKENS);
                    if (!authTokens) return [3 /*break*/, 6];
                    authTokensJson = JSON.parse(decodeBase64Str(authTokens));
                    if (!authTokensJson.refresh_token) return [3 /*break*/, 5];
                    isRefreshing = true;
                    _a.label = 1;
                case 1:
                    _a.trys.push([1, 3, , 4]);
                    return [4 /*yield*/, axios.request({
                            baseURL: getOAuthIssuer(),
                            url: "/oauth2/token",
                            method: "POST",
                            headers: {
                                "Content-Type": "application/x-www-form-urlencoded",
                                Authorization: "Basic ".concat(base64Str("".concat(import.meta.env.VITE_OAUTH_CLIENT_ID, ":").concat(import.meta.env.VITE_OAUTH_CLIENT_SECRET))),
                            },
                            data: {
                                grant_type: REFRESH_TOKEN,
                                refresh_token: authTokensJson.refresh_token,
                            },
                        })];
                case 2:
                    res_1 = _a.sent();
                    localStorage.setItem(AUTH_TOKENS, base64Str(JSON.stringify(res_1)));
                    // 重试刷新 token 间的所有请求
                    retryRequests.forEach(function (cb) { return cb(res_1); });
                    isRefreshing = false;
                    retryRequests = [];
                    // 重试当前请求
                    resetToken(res_1, requestConfig);
                    return [2 /*return*/, axios(requestConfig)];
                case 3:
                    err_1 = _a.sent();
                    Notification.error("刷新 token 失败");
                    isRefreshing = false;
                    retryRequests = [];
                    localStorage.removeItem(AUTH_TOKENS);
                    handleRelogin();
                    return [3 /*break*/, 4];
                case 4: return [3 /*break*/, 6];
                case 5:
                    isRefreshing = false;
                    retryRequests = [];
                    localStorage.removeItem(AUTH_TOKENS);
                    handleRelogin();
                    _a.label = 6;
                case 6: return [3 /*break*/, 8];
                case 7: return [2 /*return*/, new Promise(function (resolve) {
                        // 等待刷新 token 完成后执行
                        retryRequests.push(function (token) {
                            resetToken(token, requestConfig);
                            resolve(axios(requestConfig));
                        });
                    })];
                case 8: return [2 /*return*/];
            }
        });
    });
}
/**
 * 重新登录
 */
function handleRelogin() {
    // 存储当前页面路径及 query，方便登录后跳转
    var currentRoute = router.currentRoute.value;
    if (currentRoute && !localStorage.getItem(REDIRECT_PATH)) {
        localStorage.setItem(REDIRECT_PATH, currentRoute.path);
        if (currentRoute.query && !localStorage.getItem(REDIRECT_QUERY)) {
            localStorage.setItem(REDIRECT_QUERY, JSON.stringify(currentRoute.query));
        }
    }
    router.push({
        path: "/oauth2/redirect",
    });
}
/**
 * 重新设置token
 */
function resetToken(token, requestConfig) {
    requestConfig.headers.Authorization = "".concat(token.token_type, " ").concat(token.access_token);
}
// 默认导出Request实例
export default Request;
