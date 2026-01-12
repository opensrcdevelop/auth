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
import { createIdentitySource, getIdentitySourceProviderList, } from "@/api/identitySource";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { generateRandomString, getOAuthIssuer, handleApiError, handleApiSuccess, } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { computed, defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 身份源提供商列表 */
var providerList = reactive([]);
/**
 * 获取身份源提供商列表
 */
var handleGetProviderList = function () {
    getIdentitySourceProviderList({
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            providerList.length = 0;
            providerList.push.apply(providerList, data.list);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取身份源提供商列表");
    });
};
/**
 * 身份源提供商
 */
var identitySourceProviderRef = ref(null);
var identitySourceProvider = reactive({
    id: undefined,
    name: undefined,
});
/**
 * 创建身份源表单
 */
var createIdentitySourceForm = reactive({
    providerId: undefined,
    name: undefined,
    code: undefined,
    clientId: undefined,
    clientSecret: undefined,
    clientAuthenticationMethod: "client_secret_basic",
    authorizationGrantType: "authorization_code",
    additionalParams: undefined,
});
var createIdentitySourceFormRef = ref();
var createIdentitySourceFormRules = {
    name: [{ required: true, message: "身份源显示名称未填写" }],
    code: [{ required: true, message: "身份源标识未填写" }],
    clientId: [{ required: true, message: "Client ID 未填写" }],
    clientSecret: [{ required: true, message: "Client Secret 未填写" }],
    clientAuthenticationMethod: [
        { required: true, message: "客户端认证方式未选择" },
    ],
    authorizationGrantType: [{ required: true, message: "授权类型未选择" }],
    additionalParams: [
        {
            validator: function (value, cb) {
                try {
                    if (value) {
                        JSON.parse(value);
                        cb();
                    }
                }
                catch (e) {
                    cb("额外参数 JSON 格式错误");
                }
            },
        },
    ],
};
// 回调地址
var callBackUrl = computed(function () {
    return "".concat(getOAuthIssuer()).concat(import.meta.env.VITE_API_BASE_URI, "/login/federation/callback/").concat(createIdentitySourceForm.code ? createIdentitySourceForm.code : "身份源标识");
});
/**
 * 生成随机身份源标识
 */
var generateRandomIdentitySourceCode = function () {
    createIdentitySourceForm.code = generateRandomString(10);
};
/**
 * 重置创建身份源表单
 */
var handleResetCreateIdentitySourceForm = function () {
    if (identitySourceProviderRef.value) {
        identitySourceProviderRef.value.resetFields();
        handleGetProviderList();
    }
    createIdentitySourceFormRef.value.resetFields();
};
/**
 * 提交创建身份源表单
 */
var handleCreateIdentitySourceFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var hasError, requestData;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                hasError = false;
                if (!identitySourceProviderRef.value) return [3 /*break*/, 2];
                return [4 /*yield*/, identitySourceProviderRef.value.validate(function (errors) {
                        if (errors) {
                            hasError = true;
                        }
                    })];
            case 1:
                _a.sent();
                _a.label = 2;
            case 2: return [4 /*yield*/, createIdentitySourceFormRef.value.validate(function (errors) {
                    if (errors) {
                        hasError = true;
                    }
                })];
            case 3:
                _a.sent();
                if (hasError) {
                    return [2 /*return*/];
                }
                requestData = __assign(__assign({}, createIdentitySourceForm), { providerId: identitySourceProvider.id });
                if (requestData.additionalParams) {
                    requestData.additionalParams = JSON.parse(createIdentitySourceForm.additionalParams);
                }
                else {
                    requestData.additionalParams = undefined;
                }
                createIdentitySource(requestData)
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("创建成功");
                        handleResetCreateIdentitySourceForm();
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "创建身份源");
                });
                return [2 /*return*/];
        }
    });
}); };
export default defineComponent({
    setup: function () {
        onMounted(function () {
            var globalVariables = useGlobalVariablesStore().getData();
            if (globalVariables.identitySourceProvider.id &&
                globalVariables.identitySourceProvider.name) {
                identitySourceProvider.id = globalVariables.identitySourceProvider.id;
                identitySourceProvider.name =
                    globalVariables.identitySourceProvider.name;
            }
            else {
                identitySourceProvider.id = undefined;
                identitySourceProvider.name = undefined;
                handleGetProviderList();
            }
        });
        return {
            handleBack: handleBack,
            identitySourceProviderRef: identitySourceProviderRef,
            identitySourceProvider: identitySourceProvider,
            createIdentitySourceForm: createIdentitySourceForm,
            createIdentitySourceFormRef: createIdentitySourceFormRef,
            createIdentitySourceFormRules: createIdentitySourceFormRules,
            callBackUrl: callBackUrl,
            generateRandomIdentitySourceCode: generateRandomIdentitySourceCode,
            handleResetCreateIdentitySourceForm: handleResetCreateIdentitySourceForm,
            handleCreateIdentitySourceFormSubmit: handleCreateIdentitySourceFormSubmit,
            providerList: providerList,
        };
    },
});
