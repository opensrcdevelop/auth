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
import { debugPermissionExp, getPermissionExpDetail, getPermissionExpPermissions, getPermissionExpTemplateList, getPremissionExpTemplateParamConfigs, removeAuthorizeCondition, updatePermissionExp, } from "@/api/permission";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import ParamInput from "../components/ParamInput.vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("condition_info");
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
    if (id === void 0) { id = permissionExpId.value; }
    switch (tabKey) {
        case "condition_info":
            handleGetPermissionExpDetail(id);
            break;
        case "permission_list":
            if (!permissionExpId.value) {
                handleGetPermissionExpDetail(id);
            }
            handleGetPermissionExpPermissions(id);
            break;
    }
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
var permissionExpId = ref("");
var permissionExpName = ref("");
/** 权限表达式表单 */
var templateParamsRef = ref(null);
var permissionExpInfoFormRef = ref();
var permissionExpInfoForm = reactive({
    id: undefined,
    name: undefined,
    templateId: undefined,
    templateParams: undefined,
    expression: undefined,
    desc: undefined,
});
var permissionExpInfoFormRules = {
    name: [{ required: true, message: "限制条件名称未填写" }],
    expression: [{ required: true, message: "JEXL 表达式未填写" }],
};
/**
 * 获取权限表达式详情
 */
var handleGetPermissionExpDetail = function (id) { return __awaiter(void 0, void 0, void 0, function () {
    var result, err_1;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, getPermissionExpDetail(id)];
            case 1:
                result = _a.sent();
                handleApiSuccess(result, function (data) {
                    permissionExpId.value = data.id;
                    permissionExpName.value = data.name;
                    permissionExpInfoForm.id = data.id;
                    permissionExpInfoForm.name = data.name;
                    permissionExpInfoForm.templateId = data.templateId;
                    permissionExpInfoForm.templateParams = data.templateParams;
                    permissionExpInfoForm.expression = data.expression;
                    permissionExpInfoForm.desc = data.desc;
                });
                return [3 /*break*/, 3];
            case 2:
                err_1 = _a.sent();
                handleApiError(err_1, "获取权限表达式详情");
                return [3 /*break*/, 3];
            case 3:
                if (!permissionExpInfoForm.templateId) return [3 /*break*/, 5];
                return [4 /*yield*/, handleGetParamConfigs(permissionExpInfoForm.templateId)];
            case 4:
                _a.sent();
                _a.label = 5;
            case 5: return [2 /*return*/];
        }
    });
}); };
/** 权限 */
var permissions = reactive([]);
/**
 * 获取权限表达式关联的权限表达式
 */
var handleGetPermissionExpPermissions = function (id) {
    getPermissionExpPermissions(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            permissions.length = 0;
            permissions.push.apply(permissions, data);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取权限表达式关联的权限列表");
    });
};
/** 模板参数配置 */
var templateParamConfigs = reactive([]);
var handleGetParamConfigs = function (templateId) { return __awaiter(void 0, void 0, void 0, function () {
    var result, err_2;
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
                    permissionExpInfoForm.templateParams = data.map(function (item) {
                        var param = permissionExpInfoForm.templateParams.find(function (param) { return param.code === item.code; });
                        return {
                            code: item.code,
                            value: param ? param.value : item.defaultValue,
                        };
                    });
                });
                return [3 /*break*/, 3];
            case 2:
                err_2 = _a.sent();
                handleApiError(err_2, "获取限制条件模板参数配置");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
/**
 * 提交权限表达式表单
 */
var handlePermissionExpInfoFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var validateResults, useTemplate, result, isValid;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                validateResults = [];
                validateResults.push(permissionExpInfoFormRef.value.validate());
                useTemplate = permissionExpInfoForm.templateId ? true : false;
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
                updatePermissionExp(__assign(__assign({}, permissionExpInfoForm), { useTemplate: useTemplate }))
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("保存成功");
                        handleGetPermissionExpDetail(permissionExpId.value);
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "更新权限表达式");
                });
                return [2 /*return*/];
        }
    });
}); };
/**
 * 重置权限表达式表单
 */
var handleResetPermissionExpInfoForm = function () {
    permissionExpInfoFormRef.value.resetFields();
    if (permissionExpInfoForm.templateId) {
        templateParamsRef.value.reset();
    }
    handleGetPermissionExpDetail(permissionExpId.value);
};
/**
 * 删除授权条件
 *
 * @param authorizeId 授权ID
 */
var handleRemoveAuthorizeCondition = function (authorizeId) {
    Modal.warning({
        title: "确定取消限制吗？",
        content: "此操作将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeAuthorizeCondition({
                authorizeIds: [authorizeId],
                permissionExpIds: [permissionExpId.value],
            })
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("取消限制成功");
                    handleGetPermissionExpPermissions(permissionExpId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除授权条件");
            });
        },
    });
};
/**
 * 跳转被授权主体详情
 */
var handeToPrincipalDetail = function (principal) {
    if (principal.principalType === "USER") {
        handleToUserDetail(principal.principalId);
    }
    if (principal.principalType === "USER_GROUP") {
        hantoToUserGroupDetail(principal.principalId);
    }
    if (principal.principalType === "ROLE") {
        handleToRoleDetail(principal.principalId);
    }
};
/**
 * 跳转用户组详情
 */
var hantoToUserGroupDetail = function (id) {
    router.push({
        path: "/user/group/detail",
        query: {
            id: id,
            active_tab: "user_group_info",
        },
    });
};
/**
 * 跳转用户详情
 */
var handleToUserDetail = function (id) {
    router.push({
        path: "/user/detail",
        query: {
            id: id,
            active_tab: "user_info",
        },
    });
};
/**
 * 跳转角色详情
 */
var handleToRoleDetail = function (id) {
    router.push({
        path: "/role/detail",
        query: {
            id: id,
            active_tab: "role_info",
        },
    });
};
/**
 * 跳转资源组详情
 */
var handleToResourceGroupDetail = function (id) {
    router.push({
        path: "/resource/group/detail",
        query: {
            id: id,
            active_tab: "resource_group_info",
        },
    });
};
/**
 * 跳转资源详情
 */
var handleToResourceDetail = function (id) {
    router.push({
        path: "/permission/resource/detail",
        query: {
            id: id,
            active_tab: "resource_info",
        },
    });
};
/**
 * 跳转权限详情
 */
var handleToPermissionDetail = function (id) {
    router.push({
        path: "/permission/detail",
        query: {
            id: id,
            active_tab: "permission_info",
        },
    });
};
/** 调试运行弹框 */
var debugDrawerVisible = ref(false);
var debugFormRef = ref();
var debugForm = reactive({
    expressionId: undefined,
    useTemplate: false,
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
    debugForm.expressionId = permissionExpId.value;
    debugDrawerVisible.value = true;
};
/**
 * 关闭调试运行弹框
 */
var handleCloseDebugDrawer = function () {
    debugFormRef.value.resetFields();
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
                return [4 /*yield*/, Promise.all(validateResults)];
            case 1:
                result = _a.sent();
                isValid = result.filter(function (item) { return item; }).length === 0;
                if (!isValid) {
                    return [2 /*return*/];
                }
                debugFormSubmitLoading.value = true;
                debugPermissionExp({
                    expressionId: debugForm.expressionId,
                    useTemplate: debugForm.useTemplate,
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
        ParamInput: ParamInput,
    },
    setup: function () {
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "condition_info";
            templateParamConfigs.length = 0;
            handleGetTemplateList();
            handleTabInit(activeTab.value, getQueryString("id"));
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            templateList: templateList,
            templateParamConfigs: templateParamConfigs,
            permissionExpId: permissionExpId,
            permissionExpName: permissionExpName,
            templateParamsRef: templateParamsRef,
            permissionExpInfoFormRef: permissionExpInfoFormRef,
            permissionExpInfoForm: permissionExpInfoForm,
            permissionExpInfoFormRules: permissionExpInfoFormRules,
            permissions: permissions,
            handeToPrincipalDetail: handeToPrincipalDetail,
            handleToResourceGroupDetail: handleToResourceGroupDetail,
            handleToResourceDetail: handleToResourceDetail,
            handleToPermissionDetail: handleToPermissionDetail,
            handleRemoveAuthorizeCondition: handleRemoveAuthorizeCondition,
            handlePermissionExpInfoFormSubmit: handlePermissionExpInfoFormSubmit,
            handleResetPermissionExpInfoForm: handleResetPermissionExpInfoForm,
            debugDrawerVisible: debugDrawerVisible,
            debugFormRef: debugFormRef,
            debugForm: debugForm,
            debugFormRules: debugFormRules,
            handleOpenDebugDrawer: handleOpenDebugDrawer,
            handleCloseDebugDrawer: handleCloseDebugDrawer,
            handleDebugFormSubmit: handleDebugFormSubmit,
            debugFormSubmitLoading: debugFormSubmitLoading,
            debugResult: debugResult,
            debugResultModalVisible: debugResultModalVisible,
        };
    },
});
