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
import { defineComponent, reactive, ref } from "vue";
import ParamSelect from "../components/ParamSelect.vue";
import { Modal, Notification } from "@arco-design/web-vue";
import { createPermissionExpTemplate } from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/**
 * 模板信息表单
 */
var templateInfoFormRef = ref();
var paramConfigRefs = ref([]);
var templateInfoForm = reactive({
    name: undefined,
    desc: undefined,
    expression: undefined,
    paramConfigs: [],
});
var templateInfoFormRules = {
    name: [{ required: true, message: "模板名称未填写" }],
    desc: [{ required: true, message: "模板描述未填写" }],
    expression: [{ required: true, message: "JEXL 表达式未填写" }],
};
/**
 * 模板参数类型
 */
var parmaTypes = [
    { value: "STRING", label: "字符串" },
    { value: "NUMBER", label: "数值" },
    { value: "BOOLEAN", label: "布尔值" },
    { value: "LIST", label: "列表" },
    { value: "CHOICE", label: "选择" },
];
/**
 * 添加参数配置对话框
 */
var addParamConfigModalVisible = ref(false);
var selectedParamType = ref("STRING");
/**
 * 打开添加参数配置对话框
 */
var handleParamConfigModalOpen = function () {
    addParamConfigModalVisible.value = true;
    selectedParamType.value = "STRING";
};
/**
 * 关闭添加参数配置对话框
 */
var handleParamConfigModalClose = function () {
    addParamConfigModalVisible.value = false;
    selectedParamType.value = "STRING";
};
/**
 * 添加参数配置
 */
var handleParamConfigModalConfirm = function () {
    var paramConfig = {
        name: undefined,
        type: selectedParamType.value,
        required: false,
        defaultValue: undefined,
        options: undefined,
        multiple: undefined,
        code: undefined,
    };
    if (selectedParamType.value === "CHOICE") {
        paramConfig.options = [undefined];
        paramConfig.multiple = false;
    }
    templateInfoForm.paramConfigs.push(paramConfig);
};
/**
 * 移除参数配置
 */
var handleRemoveParamConfig = function (index) {
    Modal.confirm({
        title: "\u786E\u5B9A\u5220\u9664\u300C\u6A21\u677F\u53C2\u6570 - ".concat(index + 1, "\u300D\u5417\uFF1F"),
        content: "",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            templateInfoForm.paramConfigs.splice(index, 1);
        },
    });
};
/**
 * 重置模板信息表单
 */
var handleResetTemplateInfoForm = function () {
    templateInfoFormRef.value.resetFields();
    paramConfigRefs.value.forEach(function (paramConfig) { return paramConfig.reset(); });
};
/**
 * 提交模板信息表单
 */
var handleTemplateInfoFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var validateResults, result, isValid;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                validateResults = [];
                validateResults.push(templateInfoFormRef.value.validate());
                paramConfigRefs.value.forEach(function (paramConfig) {
                    return validateResults.push(paramConfig.validate());
                });
                return [4 /*yield*/, Promise.all(validateResults)];
            case 1:
                result = _a.sent();
                isValid = result.filter(function (item) { return item; }).length === 0;
                if (!isValid) {
                    return [2 /*return*/];
                }
                createPermissionExpTemplate(templateInfoForm)
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("创建成功");
                        handleResetTemplateInfoForm();
                        templateInfoForm.paramConfigs = [];
                        paramConfigRefs.value = [];
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "创建权限表达式模板");
                });
                return [2 /*return*/];
        }
    });
}); };
export default defineComponent({
    components: {
        ParamSelect: ParamSelect,
    },
    setup: function () {
        return {
            handleBack: handleBack,
            templateInfoForm: templateInfoForm,
            templateInfoFormRules: templateInfoFormRules,
            templateInfoFormRef: templateInfoFormRef,
            paramConfigRefs: paramConfigRefs,
            addParamConfigModalVisible: addParamConfigModalVisible,
            parmaTypes: parmaTypes,
            selectedParamType: selectedParamType,
            handleParamConfigModalOpen: handleParamConfigModalOpen,
            handleParamConfigModalClose: handleParamConfigModalClose,
            handleParamConfigModalConfirm: handleParamConfigModalConfirm,
            handleRemoveParamConfig: handleRemoveParamConfig,
            handleTemplateInfoFormSubmit: handleTemplateInfoFormSubmit,
            handleResetTemplateInfoForm: handleResetTemplateInfoForm,
        };
    },
});
