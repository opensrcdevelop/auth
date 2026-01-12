import router from "@/router";
import { computed, defineComponent, reactive, ref } from "vue";
import { MODEL_PROVIDER_TYPE_LIST } from "../constants";
import { createModelProvider } from "@/api/chatbi";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var modelProviderTypeList = MODEL_PROVIDER_TYPE_LIST;
/**
 * 创建模型提供商表单
 */
var createModelProviderFormRef = ref();
var createModelProviderFormRules = {
    name: [{ required: true, message: "模型提供商名称未填写" }],
    type: [{ required: true, message: "模型提供商类型未选择" }],
    baseUrl: [{ required: true, message: "API 接入地址未填写" }],
    optionalModels: [{ required: true, message: "可选模型未填写" }],
    defaultModel: [{ required: true, message: "默认模型未选择" }],
};
var createModelProviderForm = reactive({
    name: undefined,
    type: undefined,
    baseUrl: undefined,
    apiKey: undefined,
    temperature: undefined,
    maxTokens: undefined,
    defaultModel: undefined,
    optionalModels: undefined,
});
/**
 * 可选模型
 */
var selectableModels = computed(function () {
    var _a;
    return (_a = createModelProviderForm.optionalModels) === null || _a === void 0 ? void 0 : _a.split("\n").filter(function (item) { return item !== ""; });
});
/**
 * 提交模型提供商表单
 */
var handleCreateModelProviderFormSubmit = function (formData) {
    var _a;
    createModelProvider({
        name: formData.name,
        type: formData.type,
        baseUrl: formData.baseUrl,
        apiKey: formData.apiKey,
        temperature: formData.temperature,
        maxTokens: formData.maxTokens,
        defaultModel: formData.defaultModel,
        optionalModels: (_a = createModelProviderForm.optionalModels) === null || _a === void 0 ? void 0 : _a.split("\n").filter(function (item) { return item !== ""; }),
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("创建成功");
            handleResetCreateModelProviderForm();
        });
    })
        .catch(function (err) {
        handleApiError(err, "创建模型提供商");
    });
};
/**
 * 重置模型提供商表单
 */
var handleResetCreateModelProviderForm = function () {
    createModelProviderFormRef.value.resetFields();
};
export default defineComponent({
    setup: function () {
        return {
            handleBack: handleBack,
            modelProviderTypeList: modelProviderTypeList,
            createModelProviderFormRef: createModelProviderFormRef,
            createModelProviderFormRules: createModelProviderFormRules,
            createModelProviderForm: createModelProviderForm,
            selectableModels: selectableModels,
            handleCreateModelProviderFormSubmit: handleCreateModelProviderFormSubmit,
            handleResetCreateModelProviderForm: handleResetCreateModelProviderForm,
        };
    },
});
