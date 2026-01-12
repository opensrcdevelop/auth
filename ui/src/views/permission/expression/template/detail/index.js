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
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
import { debugPermissionExp, getPermissionExpTemplateDetail, getPermissionExpTemplateExpList, updatePermissionExpTemplate, } from "@/api/permission";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
import ParamSelect from "../components/ParamSelect.vue";
import ParamInput from "../../components/ParamInput.vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("template_info");
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
/**
 * tab 初始化
 */
var handleTabInit = function (tabKey, id) {
    if (id === void 0) { id = templateId.value; }
    switch (tabKey) {
        case "template_info":
            handleGetTemplateDetail(id);
            break;
        case "expression_list":
            if (!templateId) {
                handleGetTemplateDetail(id);
            }
            handleGetExpList(id);
            break;
    }
};
var templateName = ref(null);
var templateId = ref(null);
/**
 * 模板信息表单
 */
var templateInfoFormRef = ref();
var paramConfigRefs = ref([]);
var templateInfoForm = reactive({
    id: undefined,
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
 * 获取模板详情
 */
var handleGetTemplateDetail = function () {
    var args_1 = [];
    for (var _i = 0; _i < arguments.length; _i++) {
        args_1[_i] = arguments[_i];
    }
    return __awaiter(void 0, __spreadArray([], args_1, true), void 0, function (id) {
        var result, err_1;
        if (id === void 0) { id = templateId.value; }
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    _a.trys.push([0, 2, , 3]);
                    return [4 /*yield*/, getPermissionExpTemplateDetail(id)];
                case 1:
                    result = _a.sent();
                    handleApiSuccess(result, function (data) {
                        templateName.value = data.name;
                        templateId.value = data.id;
                        templateInfoForm.id = data.id;
                        templateInfoForm.name = data.name;
                        templateInfoForm.desc = data.desc;
                        templateInfoForm.expression = data.expression;
                        templateInfoForm.paramConfigs = data.paramConfigs;
                        paramConfigRefs.value = [];
                    });
                    return [3 /*break*/, 3];
                case 2:
                    err_1 = _a.sent();
                    handleApiError(err_1, "获取权限表达式模板详情");
                    return [3 /*break*/, 3];
                case 3: return [2 /*return*/];
            }
        });
    });
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
        multiple: undefined,
        options: undefined,
        code: undefined,
    };
    if (selectedParamType.value === "CHOICE") {
        paramConfig.options = [undefined];
        paramConfig.multiple = false;
    }
    if (!templateInfoForm.paramConfigs) {
        templateInfoForm.paramConfigs = [];
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
    handleGetTemplateDetail();
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
                updatePermissionExpTemplate(templateInfoForm)
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("保存成功");
                        handleGetTemplateDetail();
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "更新权限表达式模板");
                });
                return [2 /*return*/];
        }
    });
}); };
/**
 * 关联的权限表达式列表
 */
var expList = reactive([]);
var handleGetExpList = function (id) {
    if (id === void 0) { id = templateId.value; }
    getPermissionExpTemplateExpList(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            expList.length = 0;
            expList.push.apply(expList, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取模板关联的权限表达式列表");
    });
};
/**
 * 跳转至权限表达式详情
 */
var handleToExpDetail = function (exp) {
    router.push({
        path: "/permission/expression/detail",
        query: {
            id: exp.id,
        },
    });
};
/** 调试运行弹框 */
var debugDrawerVisible = ref(false);
var debugParamConfigs = reactive([]);
var debugParamRef = ref();
var debugFormRef = ref();
var debugForm = reactive({
    templateId: undefined,
    useTemplate: true,
    templateParams: [],
    context: undefined,
});
var debugFormRules = {
    context: {
        validator: function (value, cb) {
            if (value) {
                try {
                    var valueObj = JSON.parse(value);
                    if (typeof valueObj !== "object") {
                        cb("上下文 JSON 字符串必须是一个对象");
                    }
                }
                catch (err) {
                    cb("请检查 JSON 格式是否正确");
                }
            }
            else {
                cb();
            }
        },
    },
};
/**
 * 打开调试运行弹框
 */
var handleOpenDebugDrawer = function () {
    var _a;
    debugForm.templateId = templateId.value;
    debugForm.templateParams.length = 0;
    debugParamConfigs.length = 0;
    if (templateInfoForm.paramConfigs) {
        debugParamConfigs.push.apply(debugParamConfigs, templateInfoForm.paramConfigs);
        var params = templateInfoForm.paramConfigs.map(function (item) {
            return {
                code: item.code,
                value: item.defaultValue,
            };
        });
        (_a = debugForm.templateParams).push.apply(_a, params);
    }
    debugDrawerVisible.value = true;
};
/**
 * 关闭调试运行弹框
 */
var handleCloseDebugDrawer = function () {
    debugFormRef.value.resetFields();
    debugParamRef.value.reset();
    debugDrawerVisible.value = false;
};
/** 调试运行结果 */
var debugResult = reactive({
    success: undefined,
    execResult: undefined,
});
var debugResultModalVisible = ref(false);
/**
 * 提交调试运行表单
 */
var debugFormSubmitLoading = ref(false);
var handleDebugFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var validateResults, result, isValid;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                validateResults = [];
                validateResults.push(debugFormRef.value.validate());
                validateResults.push(debugParamRef.value.validate());
                return [4 /*yield*/, Promise.all(validateResults)];
            case 1:
                result = _a.sent();
                isValid = result.filter(function (item) { return item; }).length === 0;
                if (!isValid) {
                    return [2 /*return*/];
                }
                debugFormSubmitLoading.value = true;
                debugPermissionExp({
                    templateId: debugForm.templateId,
                    useTemplate: debugForm.useTemplate,
                    templateParams: debugForm.templateParams,
                    context: debugForm.context ? JSON.parse(debugForm.context) : {},
                })
                    .then(function (result) {
                    handleApiSuccess(result, function (data) {
                        debugResult.success = data.success;
                        debugResult.execResult = data.executeRes;
                        debugResultModalVisible.value = true;
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "调试运行限制条件模板");
                })
                    .finally(function () {
                    debugFormSubmitLoading.value = false;
                });
                return [2 /*return*/];
        }
    });
}); };
export default defineComponent({
    components: {
        ParamSelect: ParamSelect,
        ParamInput: ParamInput,
    },
    setup: function () {
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "template_info";
            handleTabInit(activeTab.value, getQueryString("id"));
        });
        return {
            handleBack: handleBack,
            templateName: templateName,
            templateId: templateId,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            templateInfoFormRef: templateInfoFormRef,
            paramConfigRefs: paramConfigRefs,
            templateInfoForm: templateInfoForm,
            templateInfoFormRules: templateInfoFormRules,
            parmaTypes: parmaTypes,
            addParamConfigModalVisible: addParamConfigModalVisible,
            selectedParamType: selectedParamType,
            handleParamConfigModalOpen: handleParamConfigModalOpen,
            handleParamConfigModalClose: handleParamConfigModalClose,
            handleParamConfigModalConfirm: handleParamConfigModalConfirm,
            handleRemoveParamConfig: handleRemoveParamConfig,
            handleTemplateInfoFormSubmit: handleTemplateInfoFormSubmit,
            handleResetTemplateInfoForm: handleResetTemplateInfoForm,
            expList: expList,
            handleToExpDetail: handleToExpDetail,
            debugDrawerVisible: debugDrawerVisible,
            debugForm: debugForm,
            debugFormRef: debugFormRef,
            debugParamRef: debugParamRef,
            debugFormRules: debugFormRules,
            debugParamConfigs: debugParamConfigs,
            handleOpenDebugDrawer: handleOpenDebugDrawer,
            handleCloseDebugDrawer: handleCloseDebugDrawer,
            debugFormSubmitLoading: debugFormSubmitLoading,
            handleDebugFormSubmit: handleDebugFormSubmit,
            debugResult: debugResult,
            debugResultModalVisible: debugResultModalVisible,
        };
    },
});
