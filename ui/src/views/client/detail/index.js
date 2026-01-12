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
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { deleteClient, getClientDetail, getOidcEndpointInfo, updateClientDetail, updateClientSecret, } from "@/api/client";
import { useRoute } from "vue-router";
import { getOAuthIssuer, handleApiError, handleApiSuccess } from "@/util/tool";
import { Message, Modal, Notification } from "@arco-design/web-vue";
import { createOidcClaim, createOidcScope, deleteOidcClaim, deleteOidcScope, getOidcClaims, getOidcScopes, updateOidcClaim, updateOidcScope, } from "@/api/oidc";
import { getUserAttrs } from "@/api/user";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("client_setting");
/**
 * tab 切换事件
 *
 * @param tabKey tabKey
 */
var handleTabChange = function (tabKey) {
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
};
/** 基本信息 */
var clientBasicInfoForm = reactive({
    id: "",
    name: "",
    desc: "",
});
var clientBasicInfoFormRules = {
    name: [
        {
            required: true,
            message: "客户端名称未填写",
        },
    ],
};
var clientBasicInfoFormRef = ref();
var clientName = ref("");
/** 端点信息 */
var clientEndpointInfo = reactive({
    id: "",
    issuer: "",
    openidConfiguration: "".concat(getOAuthIssuer(), "/.well-known/openid-configuration"),
    jwks: "",
    authorize: "",
    token: "",
    userinfo: "",
});
/** 认证信息 */
var clientAuthInfoForm = reactive({
    id: "",
    redirectUri: "",
});
var clientAuthInfoFormRules = {
    redirectUri: [
        {
            required: true,
            message: "登录回调 URL 未填写",
        },
    ],
};
var clientAuthInfoFormRef = ref();
/**
 * 获取客户端详情
 */
var handleGetClientDetail = function (id) {
    getClientDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            clientBasicInfoForm.id = data.id;
            clientBasicInfoForm.name = data.name;
            clientBasicInfoForm.desc = data.desc;
            clientName.value = data.name;
            clientEndpointInfo.id = data.id;
            clientAuthInfoForm.id = data.id;
            clientAuthInfoForm.redirectUri = data.redirectUri;
            clientAuthorizeInfoForm.id = data.id;
            clientAuthorizeInfoForm.grantTypes = data.grantTypes;
            clientAuthorizeInfoForm.scopes = data.scopes;
            clientAuthorizeInfoForm.authenticationMethods =
                data.authenticationMethods;
            clientAuthorizeInfoForm.authorizationCodeTimeToLive =
                data.authorizationCodeTimeToLive;
            clientAuthorizeInfoForm.refreshTokenTimeToLive =
                data.refreshTokenTimeToLive;
            clientAuthorizeInfoForm.accessTokenTimeToLive =
                data.accessTokenTimeToLive;
            clientAuthorizeInfoForm.requireProofKey = data.requireProofKey;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取客户端详情");
    });
};
/**
 * 获取 Oidc 端点信息
 */
var handleGetOidcEndpointInfo = function () {
    getOidcEndpointInfo()
        .then(function (result) {
        clientEndpointInfo.issuer = result.issuer;
        clientEndpointInfo.jwks = result.jwks_uri;
        clientEndpointInfo.authorize = result.authorization_endpoint;
        clientEndpointInfo.token = result.token_endpoint;
        clientEndpointInfo.userinfo = result.userinfo_endpoint;
    })
        .catch(function (err) {
        handleApiError(err, "获取 Oidc 端点信息");
    });
};
/**
 * 更新客户端基本信息
 *
 * @param formData 客户端基本信息
 */
var handleClientBasicInfoFormSubmit = function (formData) {
    updateClientDetail(formData)
        .then(function (resutlt) {
        handleApiSuccess(resutlt, function () {
            Notification.success("保存成功");
            handleGetClientDetail(formData.id);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新客户端基本信息");
    });
};
/**
 * 重置客户端基本信息表单
 */
var handleResetClientBasicInfoForm = function () {
    clientBasicInfoFormRef.value.resetFields();
    handleGetClientDetail(clientBasicInfoForm.id);
};
/**
 * 更新客户端认证配置
 *
 * @param formData 客户端认证配置
 */
var handleClientAuthInfoFormSubmit = function (formData) {
    updateClientDetail(formData)
        .then(function (resutlt) {
        handleApiSuccess(resutlt, function () {
            Notification.success("保存成功");
            handleGetClientDetail(formData.id);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新客户端认证配置");
    });
};
/**
 * 重置客户端认证配置表单
 */
var handleResetClientAuthInfoForm = function () {
    clientAuthInfoFormRef.value.resetFields();
    handleGetClientDetail(clientAuthInfoForm.id);
};
var updateClientSecretSuccessModalVisible = ref(false);
var newClientSecret = ref("");
/**
 * 刷新客户端密钥
 *
 * @param id 客户端 ID
 */
var handleUpdateClientSecretSubmit = function (id) {
    updateClientSecret(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            newClientSecret.value = data.secret;
            updateClientSecretSuccessModalVisible.value = true;
        });
    })
        .catch(function (err) {
        handleApiError(err, "刷新客户端密钥");
    });
};
var clientAuthorizeInfoForm = reactive({
    id: "",
    grantTypes: [],
    authenticationMethods: [],
    scopes: [],
    authorizationCodeTimeToLive: 0,
    accessTokenTimeToLive: 0,
    refreshTokenTimeToLive: 0,
    requireProofKey: false,
});
var clientAuthorizeInfoFormRules = {
    grantTypes: [
        {
            required: true,
            message: "至少选择一项",
        },
    ],
    authenticationMethods: [
        {
            required: true,
            message: "至少选择一项",
        },
    ],
    authorizationCodeTimeToLive: [
        {
            required: true,
            message: "授权码过期时间未填写",
        },
    ],
    accessTokenTimeToLive: [
        {
            required: true,
            message: "访问令牌过期时间未填写",
        },
    ],
    refreshTokenTimeToLive: [
        {
            required: true,
            message: "刷新令牌过期时间未填写",
        },
    ],
};
var authorizationCodeTimeToLiveUnit = ref(1);
var accessTokenTimeToLiveUnit = ref(1);
var refreshTokenTimeToLiveUnit = ref(1);
var clientAuthorizeInfoFormRef = ref();
/**
 * 更新客户端授权配置
 *
 * @param formData 客户端授权配置表单
 */
var handleClientAuthorizeInfoFormSubmit = function (formData) {
    formData.authorizationCodeTimeToLive =
        formData.authorizationCodeTimeToLive *
            authorizationCodeTimeToLiveUnit.value;
    formData.accessTokenTimeToLive =
        formData.accessTokenTimeToLive * accessTokenTimeToLiveUnit.value;
    formData.refreshTokenTimeToLive =
        formData.refreshTokenTimeToLive * refreshTokenTimeToLiveUnit.value;
    updateClientDetail(formData)
        .then(function (resutlt) {
        handleApiSuccess(resutlt, function () {
            Notification.success("保存成功");
            authorizationCodeTimeToLiveUnit.value = 1;
            accessTokenTimeToLiveUnit.value = 1;
            refreshTokenTimeToLiveUnit.value = 1;
            handleGetClientDetail(formData.id);
            handleGetOidcScopes();
            handleGetOidcClaims();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新客户端授权配置");
    });
};
/**
 * 重置客户端授权配置
 */
var handleResetClientAuthorizeInfoForm = function () {
    clientAuthorizeInfoFormRef.value.resetFields();
    handleGetClientDetail(clientAuthInfoForm.id);
    handleGetOidcScopes();
};
var oidcScopes = reactive([]);
/**
 * 获取 OIDC Scope
 */
var handleGetOidcScopes = function () {
    getOidcScopes()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            oidcScopes.length = 0;
            oidcScopes.push.apply(oidcScopes, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取 OIDC Scope");
    });
};
var oidcClaims = reactive([]);
/**
 * 获取 OIDC Claim
 */
var handleGetOidcClaims = function () {
    getOidcClaims()
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            oidcClaims.length = 0;
            oidcClaims.push.apply(oidcClaims, data);
            claimMappings.length = 0;
            claimMappings.push.apply(claimMappings, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取 OIDC Claim");
    });
};
/**
 * 保存 OIDC Scope
 *
 * @param scope OIDC scope
 */
var handleSaveOidcScopeSubmit = function (scope) {
    if (scope.name.trim() === "") {
        Message.warning("Scope 名称不能为空");
        return;
    }
    // 更新 OIDC scope
    if (scope.id) {
        updateOidcScope(scope)
            .then(function (result) {
            handleApiSuccess(result, function () {
                Notification.success("保存成功");
                handleGetOidcScopes();
            });
        })
            .catch(function (err) {
            handleApiError(err, "更新 OIDC Scope");
        });
    }
    else {
        // 创建 OIDC scope
        createOidcScope(scope)
            .then(function (result) {
            handleApiSuccess(result, function () {
                Notification.success("创建成功");
                handleGetOidcScopes();
            });
        })
            .catch(function (err) {
            handleApiError(err, "创建 OIDC Scope");
        });
    }
};
/**
 * 添加 OIDC scope
 */
var handleCreateOidcScope = function () {
    var scope = {
        name: "",
        claims: [],
    };
    oidcScopes.push(scope);
};
/**
 * 移除 OIDC Scope
 *
 * @param scope OIDC Scope
 */
var handleRemoveOidcScope = function (scope) {
    oidcScopes.splice(oidcScopes.indexOf(scope), 1);
};
/**
 * 删除 OIDC Scope
 *
 * @param scope OIDC Scope
 */
var handleDeleteOidcScopeSubmit = function (scope) {
    deleteOidcScope(scope.id)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("删除成功");
            handleGetOidcScopes();
        });
    })
        .catch(function (err) {
        handleApiError(err, "删除 OIDC Scope");
    });
};
/** 用户属性 */
var userAttrs = reactive([]);
/** claim 字段映射 */
var claimMappings = reactive([]);
/**
 * 获取所有用户属性
 */
var handleGetUserAttrs = function () {
    getUserAttrs({
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userAttrs.length = 0;
            userAttrs.push.apply(userAttrs, data.list);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户属性");
    });
};
/**
 * 添加 OIDC claim
 */
var handleCreateOidcClaim = function () {
    var claim = {
        name: "",
        userAttrId: "",
    };
    claimMappings.push(claim);
};
/**
 * 移除 OIDC Claim
 *
 * @param scope OIDC Scope
 */
var handleRemoveOidcClaim = function (claim) {
    claimMappings.splice(claimMappings.indexOf(claim), 1);
};
/**
 * 保存 OIDC Claim
 *
 * @param claim OIDC Claim
 */
var handleSaveOidcClaimSubmit = function (claim) {
    if (claim.name.trim() === "") {
        Message.warning("Claim 名称不能为空");
        return;
    }
    if (claim.userAttrId.trim() === "") {
        Message.warning("请选择一个用户字段");
        return;
    }
    // 更新 OIDC Claim
    if (claim.id) {
        updateOidcClaim(claim)
            .then(function (result) {
            handleApiSuccess(result, function () {
                Notification.success("保存成功");
                handleGetOidcClaims();
            });
        })
            .catch(function (err) {
            handleApiError(err, "更新 OIDC Claim");
        });
    }
    else {
        // 创建 OIDC Claim
        createOidcClaim(claim)
            .then(function (result) {
            handleApiSuccess(result, function () {
                Notification.success("创建成功");
                handleGetOidcClaims();
            });
        })
            .catch(function (err) {
            handleApiError(err, "创建 OIDC Claim");
        });
    }
};
/**
 * 删除 OIDC Claim
 *
 * @param scope OIDC Claim
 */
var handleDeleteOidcClaimSubmit = function (claim) {
    deleteOidcClaim(claim.id)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("删除成功");
            handleGetOidcClaims();
        });
    })
        .catch(function (err) {
        handleApiError(err, "删除 OIDC Claim");
    });
};
/**
 * 删除客户端
 *
 * @param client 客户端
 */
var handleDeleteClientSubmit = function (name, id) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u5BA2\u6237\u7AEF\u300C".concat(name, "\u300D\u5417\uFF1F"),
        content: "删除后将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteClient(id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleBack();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除客户端");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        var _this = this;
        onMounted(function () { return __awaiter(_this, void 0, void 0, function () {
            var route, clientId;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        route = useRoute();
                        if (route.query.active_tab) {
                            activeTab.value = route.query.active_tab;
                        }
                        clientId = route.query.id;
                        return [4 /*yield*/, handleGetClientDetail(clientId)];
                    case 1:
                        _a.sent();
                        return [4 /*yield*/, handleGetOidcEndpointInfo()];
                    case 2:
                        _a.sent();
                        handleGetOidcScopes();
                        handleGetOidcClaims();
                        handleGetUserAttrs();
                        return [2 /*return*/];
                }
            });
        }); });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            clientBasicInfoForm: clientBasicInfoForm,
            clientBasicInfoFormRules: clientBasicInfoFormRules,
            handleClientBasicInfoFormSubmit: handleClientBasicInfoFormSubmit,
            clientBasicInfoFormRef: clientBasicInfoFormRef,
            clientEndpointInfo: clientEndpointInfo,
            clientAuthInfoForm: clientAuthInfoForm,
            clientAuthInfoFormRef: clientAuthInfoFormRef,
            clientName: clientName,
            handleResetClientBasicInfoForm: handleResetClientBasicInfoForm,
            clientAuthInfoFormRules: clientAuthInfoFormRules,
            handleClientAuthInfoFormSubmit: handleClientAuthInfoFormSubmit,
            handleResetClientAuthInfoForm: handleResetClientAuthInfoForm,
            handleUpdateClientSecretSubmit: handleUpdateClientSecretSubmit,
            updateClientSecretSuccessModalVisible: updateClientSecretSuccessModalVisible,
            newClientSecret: newClientSecret,
            clientAuthorizeInfoForm: clientAuthorizeInfoForm,
            clientAuthorizeInfoFormRules: clientAuthorizeInfoFormRules,
            handleClientAuthorizeInfoFormSubmit: handleClientAuthorizeInfoFormSubmit,
            clientAuthorizeInfoFormRef: clientAuthorizeInfoFormRef,
            handleResetClientAuthorizeInfoForm: handleResetClientAuthorizeInfoForm,
            authorizationCodeTimeToLiveUnit: authorizationCodeTimeToLiveUnit,
            accessTokenTimeToLiveUnit: accessTokenTimeToLiveUnit,
            refreshTokenTimeToLiveUnit: refreshTokenTimeToLiveUnit,
            oidcScopes: oidcScopes,
            oidcClaims: oidcClaims,
            handleSaveOidcScopeSubmit: handleSaveOidcScopeSubmit,
            handleCreateOidcScope: handleCreateOidcScope,
            handleRemoveOidcScope: handleRemoveOidcScope,
            handleDeleteOidcScopeSubmit: handleDeleteOidcScopeSubmit,
            userAttrs: userAttrs,
            claimMappings: claimMappings,
            handleCreateOidcClaim: handleCreateOidcClaim,
            handleRemoveOidcClaim: handleRemoveOidcClaim,
            handleSaveOidcClaimSubmit: handleSaveOidcClaimSubmit,
            handleDeleteOidcClaimSubmit: handleDeleteOidcClaimSubmit,
            handleDeleteClientSubmit: handleDeleteClientSubmit,
        };
    },
});
