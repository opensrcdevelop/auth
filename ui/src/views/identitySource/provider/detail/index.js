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
import { deleteIdentitySourceProvider, getIdentitySourceProviderDetail, getIdentitySourceRegistrations, updateIdentitySource, updateIdentitySourceProvider, } from "@/api/identitySource";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
import { useRoute } from "vue-router";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("provider_info");
/**
 * tab 切换事件
 */
var handleTabChange = function (tabKey) {
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
    handleTabInit(tabKey);
};
var handleTabInit = function (tabKey) {
    switch (tabKey) {
        case "registration_list":
            handleGetProviderDetail();
            handleGetRegistrations();
            break;
        case "provider_info":
            handleGetProviderDetail();
    }
};
var providerId = ref("");
var providerName = ref("");
/**
 * 身份源提供商基本信息表单
 */
var providerBasicInfoFormRef = ref(null);
var providerBasicInfoForm = reactive({
    name: undefined,
    code: undefined,
    desc: undefined,
    logo: undefined,
    scopes: undefined,
});
var providerBasicInfoFormRules = {
    name: [{ required: true, message: "身份源提供商名称未填写" }],
    logo: [{ required: true, message: "身份源提供商 Logo 未填写" }],
    code: [
        { required: true, message: "身份源提供商标识未填写" },
        {
            validator: function (value, cb) {
                if (value && !/^[A-Za-z0-9]+$/.test(value)) {
                    cb("只允许包含英文字母、数字");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
/**
 * 重置身份源提供商基本信息表单
 */
var handleResetProviderBasicInfoForm = function () {
    providerBasicInfoFormRef.value.resetFields();
    handleGetProviderDetail();
};
/**
 * 提交身份源提供商基本信息表单
 */
var handleProviderBasicInfoFormSubmit = function (formData) {
    updateIdentitySourceProvider(__assign(__assign({}, formData), { id: providerId.value }))
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetProviderDetail();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新身份源提供商");
    });
};
/**
 * 身份源提供商端点信息表单
 */
var providerEndpointInfoFormRef = ref(null);
var providerEndpointInfoForm = reactive({
    authorizationUri: undefined,
    enableCustomAuthzReq: undefined,
    authzReqCfg: undefined,
    tokenUri: undefined,
    enableCustomTokenReq: undefined,
    tokenReqCfg: undefined,
    userInfoUris: undefined,
    enableCustomUserInfoReq: undefined,
    userInfoReqCfg: undefined,
    jwkSetUri: undefined,
    scopes: undefined,
});
var providerEndpointInfoFormRules = {
    authorizationUri: [{ required: true, message: "授权地址未填写" }],
    tokenUri: [{ required: true, message: "Token 地址未填写" }],
    userInfoUris: [{ required: true, message: "用户信息地址未填写" }],
    authzReqCfg: [
        {
            validator: function (value, cb) {
                if (providerEndpointInfoForm.enableCustomAuthzReq && !value) {
                    cb("授权请求配置未填写");
                }
                else {
                    cb();
                }
            },
        },
        {
            validator: function (value, cb) {
                try {
                    JSON.parse(value);
                    cb();
                }
                catch (err) {
                    cb("授权请求配置 JSON 格式错误");
                }
            },
        },
    ],
    tokenReqCfg: [
        {
            validator: function (value, cb) {
                if (providerEndpointInfoForm.enableCustomTokenReq && !value) {
                    cb("Token 请求配置未填写");
                }
                else {
                    cb();
                }
            },
        },
        {
            validator: function (value, cb) {
                try {
                    JSON.parse(value);
                    cb();
                }
                catch (err) {
                    cb("Token 请求配置 JSON 格式错误");
                }
            },
        },
    ],
    userInfoReqCfg: [
        {
            validator: function (value, cb) {
                if (providerEndpointInfoForm.enableCustomUserInfoReq && !value) {
                    cb("用户信息请求配置未填写");
                }
                else {
                    cb();
                }
            },
        },
        {
            validator: function (value, cb) {
                try {
                    JSON.parse(value);
                    cb();
                }
                catch (err) {
                    cb("用户信息请求配置 JSON 格式错误");
                }
            },
        },
    ],
};
/**
 * 重置身份源提供商端点信息表单
 */
var handleResetProviderEndpointInfoForm = function () {
    providerEndpointInfoFormRef.value.resetFields();
    handleGetProviderDetail();
};
/**
 * 提交身份源提供商端点信息表单
 */
var handleProviderEndpointInfoFormSubmit = function (formData) {
    var requestData = __assign(__assign({}, formData), { id: providerId.value });
    if (requestData.enableCustomAuthzReq) {
        requestData.authzReqCfg = JSON.parse(requestData.authzReqCfg);
    }
    else {
        requestData.authzReqCfg = undefined;
    }
    if (requestData.enableCustomTokenReq) {
        requestData.tokenReqCfg = JSON.parse(requestData.tokenReqCfg);
    }
    else {
        requestData.tokenReqCfg = undefined;
    }
    if (requestData.enableCustomUserInfoReq) {
        requestData.userInfoReqCfg = JSON.parse(requestData.userInfoReqCfg);
    }
    else {
        requestData.userInfoReqCfg = undefined;
    }
    updateIdentitySourceProvider(requestData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetProviderDetail();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新身份源提供商");
    });
};
/**
 * 删除用户信息端点
 */
var handleRemoveUserInfoUri = function (index) {
    providerEndpointInfoForm.userInfoUris.splice(index, 1);
};
/**
 * 添加用户信息端点
 */
var handleAddUserInfoUri = function () {
    if (providerEndpointInfoForm.userInfoUris === undefined) {
        providerEndpointInfoForm.userInfoUris = [];
    }
    providerEndpointInfoForm.userInfoUris.push("");
};
/**
 * 身份源提供商认证信息表单
 */
var providerAuthInfoFormRef = ref(null);
var providerAuthInfoForm = reactive({
    userInfoAuthenticationMethod: undefined,
    scopes: undefined,
    usernameAttribute: undefined,
    uniqueIdAttribute: undefined,
    userMatchAttribute: undefined,
});
var providerAuthInfoFormRules = {
    userInfoAuthenticationMethod: [
        { required: true, message: "用户信息认证方式未选择" },
    ],
    usernameAttribute: [{ required: true, message: "用户名属性未填写" }],
    uniqueIdAttribute: [{ required: true, message: "唯一标识属性未填写" }],
    userMatchAttribute: [{ required: true, message: "用户匹配属性未填写" }],
};
/**
 * 删除 scope
 */
var handleRemoveScope = function (index) {
    providerAuthInfoForm.scopes.splice(index, 1);
};
/**
 * 添加 scope
 */
var handleAddScope = function () {
    if (providerAuthInfoForm.scopes === undefined) {
        providerAuthInfoForm.scopes = [];
    }
    providerAuthInfoForm.scopes.push("");
};
/**
 * 重置身份源提供商认证信息表单
 */
var handleResetProviderAuthInfoForm = function () {
    providerAuthInfoFormRef.value.resetFields();
    handleGetProviderDetail();
};
/**
 * 提交身份源提供商认证信息表单
 */
var handleProviderAuthInfoFormSubmit = function (formData) {
    updateIdentitySourceProvider(__assign(__assign({}, formData), { id: providerId.value }))
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetProviderDetail();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新身份源提供商");
    });
};
/**
 * 获取身份源提供商详情
 */
var handleGetProviderDetail = function () {
    getIdentitySourceProviderDetail(providerId.value)
        .then(function (reult) {
        handleApiSuccess(reult, function (data) {
            providerName.value = data.name;
            providerId.value = data.id;
            providerBasicInfoForm.name = data.name;
            providerBasicInfoForm.code = data.code;
            providerBasicInfoForm.desc = data.desc;
            providerBasicInfoForm.logo = data.logo;
            providerBasicInfoForm.scopes = data.scopes;
            providerEndpointInfoForm.authorizationUri = data.authorizationUri;
            providerEndpointInfoForm.enableCustomAuthzReq =
                data.enableCustomAuthzReq;
            providerEndpointInfoForm.authzReqCfg = data.authzReqCfg;
            providerEndpointInfoForm.tokenUri = data.tokenUri;
            providerEndpointInfoForm.enableCustomTokenReq =
                data.enableCustomTokenReq;
            providerEndpointInfoForm.tokenReqCfg = data.tokenReqCfg;
            providerEndpointInfoForm.userInfoUris = data.userInfoUris;
            providerEndpointInfoForm.enableCustomUserInfoReq = data.enableCustomUserInfoReq;
            providerEndpointInfoForm.userInfoReqCfg = data.userInfoReqCfg;
            providerEndpointInfoForm.scopes = data.scopes;
            providerAuthInfoForm.userInfoAuthenticationMethod =
                data.userInfoAuthenticationMethod;
            providerAuthInfoForm.scopes = data.scopes;
            providerAuthInfoForm.usernameAttribute = data.usernameAttribute;
            providerAuthInfoForm.uniqueIdAttribute = data.uniqueIdAttribute;
            providerAuthInfoForm.userMatchAttribute = data.userMatchAttribute;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取身份源提供商详情");
    });
};
/**
 * 关联身份源列表
 */
var registrationList = reactive([]);
/**
 * 获取关联身份源列表
 */
var handleGetRegistrations = function () {
    getIdentitySourceRegistrations(providerId.value)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            registrationList.length = 0;
            registrationList.push.apply(registrationList, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取关联身份源列表");
    });
};
/**
 * 更新身份源状态
 */
var handleUpdateRegistrationState = function (registration) {
    updateIdentitySource({
        id: registration.id,
        enabled: registration.enabled,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("更新身份源状态成功");
            handleGetRegistrations();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新身份源状态");
    });
};
/**
 * 跳转创建身份源
 */
var handleToCreateRegistration = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.identitySourceProvider.id = providerId.value;
    globalVariables.identitySourceProvider.name = providerName.value;
    globalVariables.saveData();
    router.push({
        path: "/identitySource/create",
    });
};
/**
 * 跳转身份源详情
 */
var handleToRegistrationDetail = function (registration) {
    router.push({
        path: "/identitySource/detail",
        query: {
            id: registration.id,
            active_tab: "registration_info",
        },
    });
};
/**
 * 删除身份源提供商
 */
var handleDeleteProvider = function () {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u8EAB\u4EFD\u6E90\u63D0\u4F9B\u5546\u300C".concat(providerName.value, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该身份源提供商及关联的身份源和用户的绑定关系，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteIdentitySourceProvider(providerId.value)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleBack();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除身份源提供商");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            var route = useRoute();
            var id = route.query.id;
            var tabName = getQueryString("active_tab") || "provider_info";
            activeTab.value = tabName;
            providerId.value = id;
            handleTabInit(tabName);
        });
        return {
            handleBack: handleBack,
            providerName: providerName,
            providerId: providerId,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            providerBasicInfoForm: providerBasicInfoForm,
            providerBasicInfoFormRef: providerBasicInfoFormRef,
            providerBasicInfoFormRules: providerBasicInfoFormRules,
            handleResetProviderBasicInfoForm: handleResetProviderBasicInfoForm,
            handleProviderBasicInfoFormSubmit: handleProviderBasicInfoFormSubmit,
            providerEndpointInfoForm: providerEndpointInfoForm,
            providerEndpointInfoFormRef: providerEndpointInfoFormRef,
            providerEndpointInfoFormRules: providerEndpointInfoFormRules,
            handleResetProviderEndpointInfoForm: handleResetProviderEndpointInfoForm,
            handleProviderEndpointInfoFormSubmit: handleProviderEndpointInfoFormSubmit,
            handleRemoveUserInfoUri: handleRemoveUserInfoUri,
            handleAddUserInfoUri: handleAddUserInfoUri,
            providerAuthInfoForm: providerAuthInfoForm,
            providerAuthInfoFormRef: providerAuthInfoFormRef,
            providerAuthInfoFormRules: providerAuthInfoFormRules,
            handleResetProviderAuthInfoForm: handleResetProviderAuthInfoForm,
            handleProviderAuthInfoFormSubmit: handleProviderAuthInfoFormSubmit,
            handleRemoveScope: handleRemoveScope,
            handleAddScope: handleAddScope,
            registrationList: registrationList,
            handleUpdateRegistrationState: handleUpdateRegistrationState,
            handleToCreateRegistration: handleToCreateRegistration,
            handleToRegistrationDetail: handleToRegistrationDetail,
            handleDeleteProvider: handleDeleteProvider
        };
    },
});
