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
import router from "@/router";
import { defineComponent, onMounted, reactive, ref } from "vue";
import { MODEL_PROVIDER_TYPE_LIST } from "../constants";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { getModelProviderDetail, updateModelProvider } from "@/api/chatbi";
import { Modal, Notification } from "@arco-design/web-vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("model_provider_info");
var handleTabInit = function (tabKey, id) {
    if (id === void 0) { id = modelProviderId.value; }
    switch (tabKey) {
        case "model_provider_info":
            handleGetModelProviderDetail(id);
            break;
    }
};
var modelProviderTypeList = MODEL_PROVIDER_TYPE_LIST;
var modelProviderId = ref("");
var modelProviderName = ref("");
/**
 * 模型提供商信息
 */
var modelProviderInfoFormRef = ref();
var modelProviderInfoFormRules = {
    name: [{ required: true, message: "模型提供商名称未填写" }],
    baseUrl: [{ required: true, message: "API 接入地址未填写" }],
    defaultModel: [{ required: true, message: "默认模型未选择" }],
};
var modelProviderInfoForm = reactive({
    id: undefined,
    name: undefined,
    type: undefined,
    baseUrl: undefined,
    apiKey: undefined,
    temperature: undefined,
    maxTokens: undefined,
    defaultModel: undefined,
});
/**
 * 可选模型
 */
var optionalModelList = ref([]);
/**
 * 获取模型提供商详情
 */
var handleGetModelProviderDetail = function (id) {
    if (id === void 0) { id = modelProviderId.value; }
    getModelProviderDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            modelProviderId.value = data.id;
            modelProviderName.value = data.name;
            modelProviderInfoForm.id = data.id;
            modelProviderInfoForm.name = data.name;
            modelProviderInfoForm.type = data.type;
            modelProviderInfoForm.baseUrl = data.baseUrl;
            modelProviderInfoForm.apiKey = data.apiKey;
            modelProviderInfoForm.temperature = data.temperature;
            modelProviderInfoForm.maxTokens = data.maxTokens;
            modelProviderInfoForm.defaultModel = data.defaultModel;
            optionalModelList.value = data.optionalModels;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取模型提供商详情");
    });
};
/**
 * 提交更新模型提供商信息表单
 */
var handleModelProviderInfoFormSubmit = function (formData) {
    updateModelProvider(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetModelProviderDetail();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新模型提供商信息");
    });
};
/**
 * 重置模型提供商信息表单
 */
var handleResetModelProviderInfoForm = function () {
    modelProviderInfoFormRef.value.resetFields();
    handleGetModelProviderDetail();
};
/**
 * 可选模型列表变更
 */
var handleOptionalModelListChange = function (_data) {
    optionalModelList.value = _data;
    handleUpdateOptionalModelList();
};
/**
 * 移除可选模型
 */
var handleRemoveOptionalModel = function (model) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u6A21\u578B\u300C".concat(model.name, "\u300D\u5417\uFF1F"),
        content: "此操作将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            optionalModelList.value.splice(optionalModelList.value.indexOf(model), 1);
            handleUpdateOptionalModelList();
        },
    });
};
/**
 * 更新可选模型列表
 */
var handleUpdateOptionalModelList = function () {
    updateModelProvider({
        id: modelProviderId.value,
        optionalModels: optionalModelList.value.map(function (item) { return item.name; }),
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("更新可选模型列表成功");
            handleGetModelProviderDetail();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新可选模型列表");
    });
};
/**
 * 添加可选模型对话框
 */
var addOptionalModelModalVisible = ref(false);
var addOptionalModelForm = reactive({
    name: undefined,
});
var addOptionalModelFormRef = ref();
var addOptionalModelFormRules = {
    name: [{ required: true, message: "模型名称未填写" }],
};
/**
 * 提交添加可选模型表单
 */
var handleAddOptionalModelFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var errors;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, addOptionalModelFormRef.value.validate()];
            case 1:
                errors = _a.sent();
                if (!errors) {
                    optionalModelList.value.push({
                        name: addOptionalModelForm.name,
                        usedReqTokens: 0,
                        usedRepTokens: 0,
                    });
                    addOptionalModelModalVisible.value = false;
                    handleUpdateOptionalModelList();
                }
                else {
                    addOptionalModelModalVisible.value = true;
                }
                return [2 /*return*/];
        }
    });
}); };
/**
 * 关闭添加可选模型对话框
 */
var handleCloseAddOptionalModelModal = function () {
    addOptionalModelModalVisible.value = false;
    addOptionalModelFormRef.value.resetFields();
};
export default defineComponent({
    setup: function () {
        var modelProviderId = getQueryString("id");
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "model_provider_info";
            handleTabInit(activeTab.value, modelProviderId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            modelProviderTypeList: modelProviderTypeList,
            modelProviderId: modelProviderId,
            modelProviderName: modelProviderName,
            modelProviderInfoFormRef: modelProviderInfoFormRef,
            modelProviderInfoForm: modelProviderInfoForm,
            modelProviderInfoFormRules: modelProviderInfoFormRules,
            optionalModelList: optionalModelList,
            handleModelProviderInfoFormSubmit: handleModelProviderInfoFormSubmit,
            handleResetModelProviderInfoForm: handleResetModelProviderInfoForm,
            handleOptionalModelListChange: handleOptionalModelListChange,
            addOptionalModelModalVisible: addOptionalModelModalVisible,
            addOptionalModelForm: addOptionalModelForm,
            addOptionalModelFormRef: addOptionalModelFormRef,
            addOptionalModelFormRules: addOptionalModelFormRules,
            handleAddOptionalModelFormSubmit: handleAddOptionalModelFormSubmit,
            handleRemoveOptionalModel: handleRemoveOptionalModel,
            handleCloseAddOptionalModelModal: handleCloseAddOptionalModelModal,
        };
    },
});
