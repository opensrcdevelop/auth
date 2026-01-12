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
import { createIdentitySourceProvider } from "@/api/identitySource";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/**
 * 创建身份源提供商表单
 */
var createProviderForm = reactive({
    name: undefined,
    code: undefined,
    desc: undefined,
    logo: undefined,
    authorizationUri: undefined,
    enableCustomAuthzReq: undefined,
    authzReqCfg: undefined,
    tokenUri: undefined,
    enableCustomTokenReq: undefined,
    tokenReqCfg: undefined,
    userInfoUris: [undefined],
    jwkSetUri: undefined,
    enableCustomUserInfoReq: undefined,
    userInfoReqCfg: undefined,
    userInfoAuthenticationMethod: "header",
    scopes: [""],
    usernameAttribute: undefined,
    uniqueIdAttribute: undefined,
    userMatchAttribute: undefined,
});
var createProviderFormRef = ref(null);
var createProviderFormRules = {
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
    authorizationUri: [{ required: true, message: "授权地址未填写" }],
    tokenUri: [{ required: true, message: "Token 地址未填写" }],
    userInfoUris: [{ required: true, message: "用户信息地址未填写" }],
    userInfoAuthenticationMethod: [
        { required: true, message: "用户信息认证方式未填写" },
    ],
    usernameAttribute: [{ required: true, message: "用户名属性未填写" }],
    uniqueIdAttribute: [{ required: true, message: "唯一标识属性未填写" }],
    userMatchAttribute: [{ required: true, message: "用户匹配属性未填写" }],
    authzReqCfg: [
        {
            validator: function (value, cb) {
                if (createProviderForm.enableCustomAuthzReq && !value) {
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
                if (createProviderForm.enableCustomTokenReq && !value) {
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
                if (createProviderForm.enableCustomUserInfoReq && !value) {
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
 * 删除用户信息端点
 */
var handleRemoveUserInfoUri = function (index) {
    createProviderForm.userInfoUris.splice(index, 1);
};
/**
 * 添加用户信息端点
 */
var handleAddUserInfoUri = function () {
    if (createProviderForm.userInfoUris === undefined) {
        createProviderForm.userInfoUris = [];
    }
    createProviderForm.userInfoUris.push("");
};
/**
 * 删除 scope
 */
var handleRemoveScope = function (index) {
    createProviderForm.scopes.splice(index, 1);
};
/**
 * 添加 scope
 */
var handleAddScope = function () {
    if (createProviderForm.scopes === undefined) {
        createProviderForm.scopes = [];
    }
    createProviderForm.scopes.push("");
};
/**
 * 重置创建身份源提供商表单
 */
var handleResetCreateProviderForm = function () {
    createProviderFormRef.value.resetFields();
};
/**
 * 提交创建身份源提供商表单
 */
var handleCreateProviderFormSubmit = function (formData) {
    var requestData = __assign({}, formData);
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
    createIdentitySourceProvider(requestData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateProviderForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建身份源提供商");
    });
};
export default defineComponent({
    setup: function () {
        return {
            handleBack: handleBack,
            createProviderForm: createProviderForm,
            createProviderFormRef: createProviderFormRef,
            createProviderFormRules: createProviderFormRules,
            handleRemoveUserInfoUri: handleRemoveUserInfoUri,
            handleAddUserInfoUri: handleAddUserInfoUri,
            handleRemoveScope: handleRemoveScope,
            handleAddScope: handleAddScope,
            handleResetCreateProviderForm: handleResetCreateProviderForm,
            handleCreateProviderFormSubmit: handleCreateProviderFormSubmit,
        };
    },
});
