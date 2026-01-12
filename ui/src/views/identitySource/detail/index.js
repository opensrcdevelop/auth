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
import { getIdentitySourceDetail, getUserBindingList, updateIdentitySource, } from "@/api/identitySource";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { generateRandomString, getOAuthIssuer, getQueryString, handleApiError, handleApiSuccess, } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { computed, defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("registration_info");
var handleTabChange = function (tabKey) {
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
    handleTabInit(tabKey);
};
var handleTabInit = function (tabKey, id) {
    if (id === void 0) { id = registrationId.value; }
    switch (tabKey) {
        case "registration_info":
            handleGetRegistrationDetail(id);
            break;
        case "user_bindings":
            handleGetRegistrationDetail(id);
            handleGetUserBindingList(id);
    }
};
var registrationId = ref("");
var registrationName = ref("");
/**
 * 身份源信息表单
 */
var registrationInfoForm = reactive({
    name: undefined,
    code: undefined,
    clientId: undefined,
    clientSecret: undefined,
    clientAuthenticationMethod: undefined,
    authorizationGrantType: undefined,
    additionalParams: undefined,
});
var registrationInfoFormRef = ref(null);
var registrationInfoFormRules = {
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
    return "".concat(getOAuthIssuer()).concat(import.meta.env.VITE_API_BASE_URI, "/login/federation/callback/").concat(registrationInfoForm.code ? registrationInfoForm.code : "身份源标识");
});
/**
 * 生成随机身份源标识
 */
var generateRandomRegistraionCode = function () {
    registrationInfoForm.code = generateRandomString(10);
};
/**
 * 重置身份源信息表单
 */
var handleResetRegistrationInfoForm = function () {
    registrationInfoFormRef.value.resetFields();
    handleGetRegistrationDetail();
};
/**
 * 提交身份源信息表单
 */
var handleRegistrationInfoFormSubmit = function (formData) {
    var requestData = __assign(__assign({}, formData), { id: registrationId.value });
    if (requestData.additionalParams) {
        requestData.additionalParams = JSON.parse(formData.additionalParams);
    }
    else {
        requestData.additionalParams = undefined;
    }
    updateIdentitySource(requestData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetRegistrationDetail();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新身份源");
    });
};
/**
 * 获取身份源详情
 */
var handleGetRegistrationDetail = function (id) {
    if (id === void 0) { id = registrationId.value; }
    getIdentitySourceDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            registrationId.value = data.id;
            registrationName.value = data.name;
            registrationInfoForm.name = data.name;
            registrationInfoForm.code = data.code;
            registrationInfoForm.clientId = data.clientId;
            registrationInfoForm.clientSecret = data.clientSecret;
            registrationInfoForm.clientAuthenticationMethod =
                data.clientAuthenticationMethod;
            registrationInfoForm.authorizationGrantType =
                data.authorizationGrantType;
            registrationInfoForm.additionalParams = data.additionalParams;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取身份源详情");
    });
};
/**
 * 关联用户列表
 */
var userBindingList = reactive([]);
var userBindingSearchKeyword = ref(undefined);
var userBindingListPagination;
var handleGetUserBindingList = function (id, page, size) {
    if (id === void 0) { id = registrationId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserBindingList(id, {
        page: page,
        size: size,
        keyword: userBindingSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userBindingList.length = 0;
            userBindingList.push.apply(userBindingList, data.list);
            userBindingListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取身份源的关联用户");
    });
};
/**
 * 搜索用户绑定
 */
var handleSearchUserBinding = function () {
    handleGetUserBindingList();
};
/**
 * 跳转至用户详情
 */
var handleToUserDetail = function (id) {
    router.push({
        path: "/user/detail",
        query: {
            id: id,
        },
    });
};
export default defineComponent({
    setup: function () {
        var registrationId = getQueryString("id");
        userBindingListPagination = usePagination("".concat(registrationId, "_userBindingList"), function (_a) {
            var page = _a.page, size = _a.size;
            if (getQueryString("active_tab") === "user_bindings") {
                handleGetUserBindingList(registrationId, page, size);
            }
        });
        onMounted(function () {
            var tabName = getQueryString("active_tab") || "registration_info";
            activeTab.value = tabName;
            handleTabInit(tabName, registrationId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            registrationId: registrationId,
            registrationName: registrationName,
            registrationInfoForm: registrationInfoForm,
            registrationInfoFormRef: registrationInfoFormRef,
            registrationInfoFormRules: registrationInfoFormRules,
            callBackUrl: callBackUrl,
            generateRandomRegistraionCode: generateRandomRegistraionCode,
            handleResetRegistrationInfoForm: handleResetRegistrationInfoForm,
            handleRegistrationInfoFormSubmit: handleRegistrationInfoFormSubmit,
            userBindingList: userBindingList,
            userBindingSearchKeyword: userBindingSearchKeyword,
            userBindingListPagination: userBindingListPagination,
            handleSearchUserBinding: handleSearchUserBinding,
            handleToUserDetail: handleToUserDetail,
        };
    },
});
