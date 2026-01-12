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
import { getTenantDetail, updateTenant } from "@/api/tenant";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("tanant_info");
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
var tenantId = ref("");
var tenantName = ref("");
/** 租户信息表单 */
var tenantInfoFormRef = ref();
var tenantInfoForm = reactive({
    id: undefined,
    name: undefined,
    code: undefined,
    desc: undefined,
    enabled: undefined,
    createTime: undefined,
});
var tenantInfoFormFormRules = {
    name: [{ required: true, message: "租户名称未填写" }],
};
/** 端点信息 */
var endpointInfo = reactive({
    issuer: undefined,
    consoleUrl: undefined,
});
/**
 * 获取租户详情
 *
 * @param id 租户 ID
 */
var handleGetTenantDetail = function (id) {
    getTenantDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            tenantId.value = data.id;
            tenantName.value = data.name;
            tenantInfoForm.id = data.id;
            tenantInfoForm.name = data.name;
            tenantInfoForm.code = data.code;
            tenantInfoForm.desc = data.desc;
            tenantInfoForm.enabled = data.enabled;
            tenantInfoForm.createTime = data.createTime;
            endpointInfo.issuer = data.issuer;
            endpointInfo.consoleUrl = data.consoleUrl;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取租户详情");
    });
};
/**
 * 重置用户字段信息表单
 */
var handleResetTenantInfoForm = function () {
    tenantInfoFormRef.value.resetFields();
    handleGetTenantDetail(tenantId.value);
};
/**
 * 提交租户信息表单
 *
 * @param formData 租户信息表单
 */
var handleTenantInfoFormSubmit = function (formData) {
    delete formData.createTime;
    delete formData.code;
    updateTenant(formData)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            Notification.success("保存成功");
            handleGetTenantDetail(tenantId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新租户信息");
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "tanant_info";
            handleGetTenantDetail(getQueryString("id"));
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            tenantId: tenantId,
            tenantName: tenantName,
            tenantInfoForm: tenantInfoForm,
            tenantInfoFormFormRules: tenantInfoFormFormRules,
            tenantInfoFormRef: tenantInfoFormRef,
            handleTenantInfoFormSubmit: handleTenantInfoFormSubmit,
            handleResetTenantInfoForm: handleResetTenantInfoForm,
            endpointInfo: endpointInfo,
        };
    },
});
