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
import { createPermissionExp, getPermissionExpTemplateList, getPremissionExpTemplateParamConfigs, } from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import ParamInput from "../components/ParamInput.vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 模板列表 */
var templateList = reactive([]);
var handleGetTemplateList = function () {
    getPermissionExpTemplateList({
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            templateList.length = 0;
            templateList.push.apply(templateList, data.list);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取限制条件模板列表");
    });
};
/** 模板参数配置 */
var templateParamConfigs = reactive([]);
var handleTemplateSelectChange = function (templateId) { return __awaiter(void 0, void 0, void 0, function () {
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                if (!templateId) {
                    templateParamConfigs.length = 0;
                    createPermissionExpInfoForm.templateParams = undefined;
                    return [2 /*return*/];
                }
                return [4 /*yield*/, handleGetParamConfigs(templateId)];
            case 1:
                _a.sent();
                return [2 /*return*/];
        }
    });
}); };
var handleGetParamConfigs = function (templateId) { return __awaiter(void 0, void 0, void 0, function () {
    var result, err_1;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, getPremissionExpTemplateParamConfigs(templateId)];
            case 1:
                result = _a.sent();
                handleApiSuccess(result, function (data) {
                    templateParamConfigs.length = 0;
                    templateParamConfigs.push.apply(templateParamConfigs, data);
                    createPermissionExpInfoForm.templateParams = data.map(function (item) {
                        return {
                            code: item.code,
                            value: item.defaultValue,
                        };
                    });
                });
                return [3 /*break*/, 3];
            case 2:
                err_1 = _a.sent();
                handleApiError(err_1, "获取限制条件模板参数配置");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
/** 创建限制条件表单 */
var templateParamsRef = ref(null);
var createPermissionExpInfoForm = reactive({
    name: undefined,
    expression: undefined,
    templateId: undefined,
    templateParams: undefined,
    desc: undefined,
});
var createPermissionExpInfoFormRef = ref();
var createPermissionExpInfoFormRules = {
    name: [{ required: true, message: "限制条件名称未填写" }],
    expression: [{ required: true, message: "JEXL 表达式未填写" }],
};
/**
 * 提交创建限制条件表单
 */
var handleCreatePermissionExpInfoFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var validateResults, useTemplate, result, isValid;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                validateResults = [];
                validateResults.push(createPermissionExpInfoFormRef.value.validate());
                useTemplate = createPermissionExpInfoForm.templateId ? true : false;
                if (useTemplate) {
                    validateResults.push(templateParamsRef.value.validate());
                }
                return [4 /*yield*/, Promise.all(validateResults)];
            case 1:
                result = _a.sent();
                isValid = result.filter(function (item) { return item; }).length === 0;
                if (!isValid) {
                    return [2 /*return*/];
                }
                createPermissionExp(__assign(__assign({}, createPermissionExpInfoForm), { useTemplate: useTemplate }))
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("创建成功");
                        handleResetCreatePermissionExpInfoForm();
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "创建限制条件");
                });
                return [2 /*return*/];
        }
    });
}); };
/**
 * 重置创建限制条件表单
 */
var handleResetCreatePermissionExpInfoForm = function () {
    var templateId = createPermissionExpInfoForm.templateId;
    createPermissionExpInfoFormRef.value.resetFields();
    createPermissionExpInfoForm.templateId = templateId;
    if (templateId) {
        templateParamsRef.value.reset();
    }
};
export default defineComponent({
    components: {
        ParamInput: ParamInput,
    },
    setup: function () {
        onMounted(function () {
            handleGetTemplateList();
            createPermissionExpInfoForm.templateId = undefined;
            templateParamConfigs.length = 0;
        });
        return {
            handleBack: handleBack,
            templateList: templateList,
            templateParamsRef: templateParamsRef,
            createPermissionExpInfoForm: createPermissionExpInfoForm,
            createPermissionExpInfoFormRef: createPermissionExpInfoFormRef,
            createPermissionExpInfoFormRules: createPermissionExpInfoFormRules,
            handleCreatePermissionExpInfoFormSubmit: handleCreatePermissionExpInfoFormSubmit,
            handleResetCreatePermissionExpInfoForm: handleResetCreatePermissionExpInfoForm,
            templateParamConfigs: templateParamConfigs,
            handleTemplateSelectChange: handleTemplateSelectChange,
        };
    },
});
