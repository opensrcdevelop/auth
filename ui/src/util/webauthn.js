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
import { startAuthentication } from "@simplewebauthn/browser";
/**
 * Uint8Array 转换为 Base64URL 字符串
 */
function uint8ArrayToBase64Url(uint8Array) {
    var base64 = btoa(String.fromCharCode.apply(null, uint8Array));
    return base64.replace(/\+/g, "-").replace(/\//g, "_").replace(/=+$/, "");
}
/**
 * Uint8Array 转换为标准 Base64 字符串
 */
function uint8ArrayToBase64(uint8Array) {
    return btoa(String.fromCharCode.apply(null, uint8Array));
}
/**
 * ArrayBuffer 转换为 Base64URL 字符串
 */
function arrayBufferToBase64Url(buffer) {
    if (!buffer) {
        return "";
    }
    if (typeof buffer === "string") {
        return buffer;
    }
    var uint8Array = new Uint8Array(buffer);
    return uint8ArrayToBase64Url(uint8Array);
}
/**
 * ArrayBuffer 转换为标准 Base64 字符串（用于后端 webAuthn4j）
 */
function arrayBufferToBase64(buffer) {
    if (!buffer) {
        return "";
    }
    if (typeof buffer === "string") {
        return buffer;
    }
    var uint8Array = new Uint8Array(buffer);
    return uint8ArrayToBase64(uint8Array);
}
/**
 * 将后端返回的注册选项转换为 @simplewebauthn 期望的格式
 * 注意：@simplewebauthn/browser 会自动将 Base64URL 字符串转换为 ArrayBuffer
 */
function convertToWebAuthnOptions(options) {
    var _a, _b, _c, _d, _e, _f, _g;
    if (!options) {
        throw new Error("WebAuthn 注册选项为空");
    }
    // 处理 excludeCredentials：后端返回的是字符串数组，需要转换为对象数组
    var excludeCredentials = ((_a = options.excludeCredentials) === null || _a === void 0 ? void 0 : _a.map(function (credId) { return ({
        id: credId, // 保持字符串格式，由 @simplewebauthn 自动转换
        type: "public-key",
    }); })) || [];
    // challenge 和 user.id 保持字符串格式，由 @simplewebauthn 自动转换
    return {
        challenge: options.challenge,
        rp: {
            id: (_b = options.rp) === null || _b === void 0 ? void 0 : _b.id,
            name: (_c = options.rp) === null || _c === void 0 ? void 0 : _c.name,
        },
        user: {
            id: (_d = options.user) === null || _d === void 0 ? void 0 : _d.id,
            name: (_e = options.user) === null || _e === void 0 ? void 0 : _e.name,
            displayName: (_f = options.user) === null || _f === void 0 ? void 0 : _f.displayName,
        },
        pubKeyCredParams: ((_g = options.pubKeyCredParams) === null || _g === void 0 ? void 0 : _g.map(function (param) { return ({
            type: param.type,
            alg: param.alg,
        }); })) || [],
        timeout: options.timeout,
        authenticatorSelection: options.authenticatorSelection,
        excludeCredentials: excludeCredentials,
    };
}
/**
 * Base64URL字符串转换为ArrayBuffer
 *
 * @param base64Url Base64URL字符串
 * @returns ArrayBuffer
 */
function base64UrlToArrayBuffer(base64Url) {
    var base64 = base64Url
        .replace(/-/g, '+')
        .replace(/_/g, '/');
    var padding = '='.repeat((4 - (base64.length % 4)) % 4);
    var base64WithPadding = base64 + padding;
    var binaryString = atob(base64WithPadding);
    var bytes = new Uint8Array(binaryString.length);
    for (var i = 0; i < binaryString.length; i++) {
        bytes[i] = binaryString.charCodeAt(i);
    }
    return bytes.buffer;
}
/**
 * WebAuthn/Passkey 工具类
 */
var webauthn = {
    /**
     * 检测浏览器是否支持 WebAuthn
     */
    isSupported: function () {
        return !!(window.PublicKeyCredential &&
            typeof window.PublicKeyCredential
                .isUserVerifyingPlatformAuthenticatorAvailable === "function");
    },
    /**
     * 检测是否支持平台认证器（如 Touch ID, Windows Hello）
     */
    isPlatformAuthenticatorAvailable: function () {
        return __awaiter(this, void 0, void 0, function () {
            var _a;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0:
                        if (!window.PublicKeyCredential) {
                            return [2 /*return*/, false];
                        }
                        _b.label = 1;
                    case 1:
                        _b.trys.push([1, 3, , 4]);
                        return [4 /*yield*/, window.PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable()];
                    case 2: return [2 /*return*/, _b.sent()];
                    case 3:
                        _a = _b.sent();
                        return [2 /*return*/, false];
                    case 4: return [2 /*return*/];
                }
            });
        });
    },
    /**
     * 开始注册流程
     *
     * @param options 后端返回的注册选项
     * @returns 认证响应
     */
    startRegistration: function (options) {
        return __awaiter(this, void 0, void 0, function () {
            var webAuthnOptions, browserStartRegistration, credential, error_1;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 3, , 4]);
                        webAuthnOptions = convertToWebAuthnOptions(options);
                        return [4 /*yield*/, import("@simplewebauthn/browser")];
                    case 1:
                        browserStartRegistration = (_a.sent()).startRegistration;
                        return [4 /*yield*/, browserStartRegistration({
                                optionsJSON: webAuthnOptions,
                            })];
                    case 2:
                        credential = _a.sent();
                        return [2 /*return*/, {
                                id: credential.id,
                                rawId: arrayBufferToBase64Url(credential.rawId),
                                response: {
                                    clientDataJSON: arrayBufferToBase64Url(credential.response.clientDataJSON),
                                    attestationObject: arrayBufferToBase64(credential.response.attestationObject),
                                    transports: credential.response.transports,
                                },
                            }];
                    case 3:
                        error_1 = _a.sent();
                        console.error("WebAuthn 注册失败:", error_1);
                        throw error_1;
                    case 4: return [2 /*return*/];
                }
            });
        });
    },
    /**
     * 开始认证流程
     *
     * @param options 认证选项
     * @returns 认证响应
     */
    startAuthentication: function (options) {
        return __awaiter(this, void 0, void 0, function () {
            var credential, error_2;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        _a.trys.push([0, 2, , 3]);
                        return [4 /*yield*/, startAuthentication({ optionsJSON: options })];
                    case 1:
                        credential = _a.sent();
                        return [2 /*return*/, {
                                id: credential.id,
                                rawId: arrayBufferToBase64Url(credential.rawId),
                                response: {
                                    clientDataJSON: arrayBufferToBase64Url(credential.response.clientDataJSON),
                                    authenticatorData: arrayBufferToBase64Url(credential.response.authenticatorData),
                                    signature: arrayBufferToBase64Url(credential.response.signature),
                                    userHandle: arrayBufferToBase64Url(credential.response.userHandle),
                                },
                            }];
                    case 2:
                        error_2 = _a.sent();
                        console.error("WebAuthn 认证失败:", error_2);
                        throw error_2;
                    case 3: return [2 /*return*/];
                }
            });
        });
    },
    /**
     * 检查本地是否存在给定的凭证列表中的凭证ID
     *
     * @param credentialIds 凭证ID数组
     * @returns 是否存在匹配的凭证
     */
    hasCredentials: function (credentialIds) {
        return __awaiter(this, void 0, void 0, function () {
            var allowCredentials, credential, error_3, error_4;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        if (!credentialIds || credentialIds.length === 0) {
                            return [2 /*return*/, false];
                        }
                        _a.label = 1;
                    case 1:
                        _a.trys.push([1, 6, , 7]);
                        allowCredentials = credentialIds.map(function (id) { return ({
                            id: base64UrlToArrayBuffer(id),
                            type: "public-key",
                        }); });
                        _a.label = 2;
                    case 2:
                        _a.trys.push([2, 4, , 5]);
                        return [4 /*yield*/, navigator.credentials.get({
                                mediation: "silent",
                                publicKey: {
                                    challenge: new Uint8Array(32), // 任意challenge
                                    allowCredentials: allowCredentials,
                                    userVerification: "discouraged",
                                },
                            })];
                    case 3:
                        credential = _a.sent();
                        return [2 /*return*/, !!credential];
                    case 4:
                        error_3 = _a.sent();
                        // 如果没有匹配的凭证，会抛出错误，这是预期行为
                        return [2 /*return*/, false];
                    case 5: return [3 /*break*/, 7];
                    case 6:
                        error_4 = _a.sent();
                        console.error("检查凭证失败:", error_4);
                        return [2 /*return*/, false];
                    case 7: return [2 /*return*/];
                }
            });
        });
    },
};
export default webauthn;
