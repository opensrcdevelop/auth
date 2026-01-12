import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { createClient } from "@/api/client";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { getOidcScopes } from "@/api/oidc";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var createClientForm = reactive({
    name: "",
    desc: "",
    redirectUri: "",
    grantTypes: [],
    scopes: [],
    authenticationMethods: [],
    authorizationCodeTimeToLive: 5,
    accessTokenTimeToLive: 2,
    refreshTokenTimeToLive: 7,
    requireProofKey: false,
});
var oidcScopes = reactive([]);
var authorizationCodeTimeToLiveUnit = ref(1);
var accessTokenTimeToLiveUnit = ref(60);
var refreshTokenTimeToLiveUnit = ref(1440);
var createClientFormRef = ref();
var createClientFormRules = {
    name: [
        {
            required: true,
            message: "客户端名称未填写",
        },
    ],
    redirectUri: [
        {
            required: true,
            message: "登录回调 URL未填写",
        },
    ],
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
var createClientSuccessModalVisible = ref(false);
var clientId = ref("");
var clientSecret = ref("");
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
        handleApiError(err, "获取 OIDC Scope 失败");
    });
};
/**
 * 创建客户端
 *
 * @param formData 创建客户端表单
 */
var handleCreateClientFormSubmit = function (formData) {
    createClient({
        name: formData.name,
        desc: formData.desc,
        redirectUri: formData.redirectUri,
        grantTypes: formData.grantTypes,
        authenticationMethods: formData.authenticationMethods,
        scopes: formData.scopes,
        authorizationCodeTimeToLive: formData.authorizationCodeTimeToLive *
            authorizationCodeTimeToLiveUnit.value,
        accessTokenTimeToLive: formData.accessTokenTimeToLive * accessTokenTimeToLiveUnit.value,
        refreshTokenTimeToLive: formData.refreshTokenTimeToLive * refreshTokenTimeToLiveUnit.value,
        requireProofKey: formData.requireProofKey,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            clientId.value = result.data.id;
            clientSecret.value = result.data.secret;
            createClientSuccessModalVisible.value = true;
            handleResetCreateClientForm();
            authorizationCodeTimeToLiveUnit.value = 1;
            accessTokenTimeToLiveUnit.value = 60;
            refreshTokenTimeToLiveUnit.value = 1440;
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建客户端");
    });
};
/**
 * 重置创建客户端表单
 */
var handleResetCreateClientForm = function () {
    createClientFormRef.value.resetFields();
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            handleGetOidcScopes();
        });
        return {
            handleBack: handleBack,
            createClientForm: createClientForm,
            createClientFormRules: createClientFormRules,
            createClientFormRef: createClientFormRef,
            authorizationCodeTimeToLiveUnit: authorizationCodeTimeToLiveUnit,
            accessTokenTimeToLiveUnit: accessTokenTimeToLiveUnit,
            refreshTokenTimeToLiveUnit: refreshTokenTimeToLiveUnit,
            handleCreateClientFormSubmit: handleCreateClientFormSubmit,
            handleResetCreateClientForm: handleResetCreateClientForm,
            oidcScopes: oidcScopes,
            createClientSuccessModalVisible: createClientSuccessModalVisible,
            clientSecret: clientSecret,
            clientId: clientId,
        };
    },
});
