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
import { AUTH_TOKENS } from "@/util/constants";
import { decodeBase64Str, getOAuthIssuer } from "@/util/tool";
import { fetchEventSource } from "@microsoft/fetch-event-source";
import { onUnmounted } from "vue";
export function useEventSource() {
    var _this = this;
    var controller = null;
    var fetchStream = function (options) { return __awaiter(_this, void 0, void 0, function () {
        var headers, authTokens, token, baseUrl;
        var _this = this;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    controller = new AbortController();
                    headers = {
                        "Content-Type": "application/json",
                    };
                    authTokens = localStorage.getItem(AUTH_TOKENS);
                    if (authTokens) {
                        token = JSON.parse(decodeBase64Str(authTokens));
                        headers["Authorization"] = "".concat(token.token_type, " ").concat(token.access_token);
                    }
                    baseUrl = "".concat(getOAuthIssuer()).concat(import.meta.env.VITE_API_BASE_URI);
                    return [4 /*yield*/, fetchEventSource(baseUrl + options.url, {
                            method: options.method || "POST",
                            headers: __assign(__assign({}, headers), options.headers),
                            body: options.body ? JSON.stringify(options.body) : undefined,
                            signal: controller.signal,
                            openWhenHidden: true,
                            onopen: function (response) { return __awaiter(_this, void 0, void 0, function () {
                                var _a;
                                return __generator(this, function (_b) {
                                    if (response.ok) {
                                        (_a = options.onOpen) === null || _a === void 0 ? void 0 : _a.call(options, response);
                                        return [2 /*return*/];
                                    }
                                    throw new Error("Failed to open stream: ".concat(response.status, " ").concat(response.statusText));
                                });
                            }); },
                            onmessage: function (event) {
                                var _a, _b;
                                if (event.data) {
                                    try {
                                        var jsonData = JSON.parse(event.data);
                                        (_a = options.onMessage) === null || _a === void 0 ? void 0 : _a.call(options, jsonData);
                                    }
                                    catch (err) {
                                        (_b = options.onMessage) === null || _b === void 0 ? void 0 : _b.call(options, event.data);
                                    }
                                }
                            },
                            onerror: function (err) {
                                var _a;
                                (_a = options.onError) === null || _a === void 0 ? void 0 : _a.call(options, err);
                                throw err;
                            },
                            onclose: function () {
                                var _a;
                                (_a = options.onClose) === null || _a === void 0 ? void 0 : _a.call(options);
                            },
                        })];
                case 1:
                    _a.sent();
                    return [2 /*return*/];
            }
        });
    }); };
    var abort = function () {
        if (controller) {
            controller.abort();
            controller = null;
        }
    };
    onUnmounted(function () {
        abort();
    });
    return {
        fetchStream: fetchStream,
        abort: abort,
    };
}
