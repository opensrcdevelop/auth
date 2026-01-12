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
import { checkPassword, getPasswordPolicyDdetail, updatePasswordPolicy, } from "@/api/setting";
import { searchUser } from "@/api/user";
import { getUserGroupList } from "@/api/userGroup";
import router from "@/router";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { computed, defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    handleRestUpdatePasswordPolicyForm();
    handleResetPasswordStrengthForm();
    handleResetForceChangePasswordForm();
    handleResetCheckRes();
    router.back();
};
var passwordPolicyName = ref("");
var passwordPoliyId = ref("");
var updatePasswordPolicyForm = reactive({
    id: undefined,
    name: undefined,
    desc: undefined,
    passwordStrength: 0,
    minLength: 6,
    maxLength: 35,
    requireNumber: false,
    requireLowerCase: false,
    requireUpperCase: false,
    requireSpecialChar: false,
    minCharTypeCount: 0,
    prohibitUserInfo: false,
    prohibitSingleChar: false,
    prohibitConsecutiveChar: false,
    prohibitContainConsecutiveChar: false,
    minConsecutiveCharLength: 2,
    prohibitContainRepeatChar: false,
    minRepeatCharLength: 2,
    prohibitSpecificPassword: false,
    prohibitedPasswordList: [],
    enablePasswordDetection: false,
    enableForceChangePassword: false,
    forcedCycle: 1,
    forcedCycleUnit: "MONTH",
    remindCycle: 7,
    remindCycleUnit: "DAY",
    userIds: [],
    userGroupIds: [],
});
var handleRestUpdatePasswordPolicyForm = function () {
    updatePasswordPolicyForm.id = undefined;
    updatePasswordPolicyForm.name = undefined;
    updatePasswordPolicyForm.desc = undefined;
    updatePasswordPolicyForm.passwordStrength = 0;
    updatePasswordPolicyForm.minLength = 6;
    updatePasswordPolicyForm.maxLength = 35;
    updatePasswordPolicyForm.requireNumber = false;
    updatePasswordPolicyForm.requireLowerCase = false;
    updatePasswordPolicyForm.requireUpperCase = false;
    updatePasswordPolicyForm.requireSpecialChar = false;
    updatePasswordPolicyForm.minCharTypeCount = 0;
    updatePasswordPolicyForm.prohibitUserInfo = false;
    updatePasswordPolicyForm.prohibitSingleChar = false;
    updatePasswordPolicyForm.prohibitConsecutiveChar = false;
    updatePasswordPolicyForm.prohibitContainConsecutiveChar = false;
    updatePasswordPolicyForm.minConsecutiveCharLength = 2;
    updatePasswordPolicyForm.prohibitContainRepeatChar = false;
    updatePasswordPolicyForm.minRepeatCharLength = 2;
    updatePasswordPolicyForm.prohibitSpecificPassword = false;
    updatePasswordPolicyForm.prohibitedPasswordList = [];
    updatePasswordPolicyForm.enablePasswordDetection = false;
    updatePasswordPolicyForm.enableForceChangePassword = false;
    updatePasswordPolicyForm.forcedCycle = 1;
    updatePasswordPolicyForm.forcedCycleUnit = "MONTH";
    updatePasswordPolicyForm.remindCycle = 7;
    updatePasswordPolicyForm.remindCycleUnit = "DAY";
    updatePasswordPolicyForm.userIds = [];
    updatePasswordPolicyForm.userGroupIds = [];
};
/**
 * 基础信息
 */
var basicInfoForm = reactive({
    name: "",
    desc: "",
});
var basicInfoFormRef = ref(null);
var basicInfoFormRules = {
    name: [{ required: true, message: "策略名称未输入" }],
};
/**
 * 密码强度
 */
var passwordStrengthForm = reactive({
    passwordStrength: 0,
    minLength: 6,
    maxLength: 35,
    charType: [],
    minCharTypeCount: 0,
    prohibitUserInfo: false,
    prohibitSingleChar: false,
    prohibitConsecutiveChar: false,
    prohibitContainConsecutiveChar: false,
    minConsecutiveCharLength: 2,
    prohibitContainRepeatChar: false,
    minRepeatCharLength: 2,
    prohibitSpecificPassword: false,
    specificPasswordList: [],
    enablePasswordDetection: false,
});
var passwordStrengthFormRef = ref(null);
var passwordStrengthFormRules = {
    minLength: [{ required: true, message: "最小长度未输入" }],
    maxLength: [{ required: true, message: "最大长度未输入" }],
    charType: [{ required: true, message: "密码字符类型要求未选择" }],
};
var handleResetPasswordStrengthForm = function () {
    passwordStrengthForm.passwordStrength = 0;
    passwordStrengthForm.minLength = 6;
    passwordStrengthForm.maxLength = 35;
    passwordStrengthForm.charType = [];
    passwordStrengthForm.minCharTypeCount = 0;
    passwordStrengthForm.prohibitUserInfo = false;
    passwordStrengthForm.prohibitSingleChar = false;
    passwordStrengthForm.prohibitConsecutiveChar = false;
    passwordStrengthForm.prohibitContainConsecutiveChar = false;
    passwordStrengthForm.minConsecutiveCharLength = 2;
    passwordStrengthForm.prohibitContainRepeatChar = false;
    passwordStrengthForm.minRepeatCharLength = 2;
    passwordStrengthForm.prohibitSpecificPassword = false;
    passwordStrengthForm.specificPasswordList = [];
    passwordStrengthForm.enablePasswordDetection = false;
};
var passwordStrengthLabel = computed(function () {
    var label = "";
    switch (passwordStrengthForm.passwordStrength) {
        case 0:
            label = "无要求";
            break;
        case 1:
            label = "低强度";
            break;
        case 2:
            label = "中强度";
            break;
        case 3:
            label = "高强度";
            break;
        case 4:
            label = "自定义强度";
            break;
    }
    return label;
});
var specificPasswordListEditModalVisible = ref(false);
/**
 * 添加禁止密码
 */
var handleAddSpecificPasswordInputItem = function () {
    passwordStrengthForm.specificPasswordList.push("");
};
/**
 * 删除禁止密码
 */
var handleDeleteSpecificPasswordInputItem = function (index) {
    passwordStrengthForm.specificPasswordList.splice(index, 1);
};
/**
 * 密码轮换策略
 */
var forceChangePasswordForm = reactive({
    enableForceChangePassword: false,
    forcedCycle: 1,
    forcedCycleUnit: "MONTH",
    remindCycle: 7,
    remindCycleUnit: "DAY",
});
var forceChangePasswordFormRef = ref(null);
var forceChangePasswordFormRules = {
    forcedCycle: [{ required: true, message: "强制修改密码周期未输入" }],
    remindCycle: [{ required: true, message: "密码到期前提醒周期未输入" }],
};
var handleResetForceChangePasswordForm = function () {
    forceChangePasswordForm.enableForceChangePassword = false;
    forceChangePasswordForm.forcedCycle = 1;
    forceChangePasswordForm.forcedCycleUnit = "MONTH";
    forceChangePasswordForm.remindCycle = 7;
    forceChangePasswordForm.remindCycleUnit = "DAY";
};
/**
 * 策略应用主体
 */
var policyPrincipalForm = reactive({
    type: "USER",
    id: [],
});
var policyPrincipalFormRef = ref(null);
var policyPrincipalFormRules = {
    id: [{ required: true, message: "请选择主体" }],
};
/** 用户列表 */
var userList = reactive([]);
var userSearchKeyword = ref("");
/**
 * 获取全部用户
 */
var handleGetAllUsers = function () {
    searchUser(userSearchKeyword.value, {
        page: -1,
        size: 15,
    }).then(function (result) {
        handleApiSuccess(result, function (data) {
            userList.length = 0;
            userList.push.apply(userList, data.list);
        });
    });
};
/** 用户组列表 */
var userGroupList = reactive([]);
var userGroupSearchKeyword = ref("");
/**
 * 获取全部用户组
 */
var handleGetAllUserGroups = function () {
    getUserGroupList({
        page: -1,
        size: 15,
        keyword: userGroupSearchKeyword.value,
    }).then(function (result) {
        handleApiSuccess(result, function (data) {
            userGroupList.length = 0;
            userGroupList.push.apply(userGroupList, data.list);
        });
    });
};
/**
 * 选择主体类型变化
 */
var principalSelectChange = function (value) {
    policyPrincipalForm.id.length = 0;
    if (value === "USER") {
        handleGetAllUsers();
    }
    if (value === "USER_GROUP") {
        handleGetAllUserGroups();
    }
};
/**
 * 获取密码策略详情
 */
var handleGetPasswordPolicyDetail = function (id) { return __awaiter(void 0, void 0, void 0, function () {
    return __generator(this, function (_a) {
        return [2 /*return*/, getPasswordPolicyDdetail(id).then(function (result) {
                handleApiSuccess(result, function (data) {
                    var _a, _b;
                    passwordPolicyName.value = data.name;
                    passwordPoliyId.value = data.id;
                    // 基础信息
                    basicInfoForm.name = data.name;
                    basicInfoForm.desc = data.desc;
                    // 密码强度
                    passwordStrengthForm.passwordStrength = data.passwordStrength;
                    passwordStrengthForm.enablePasswordDetection =
                        data.enablePasswordDetection;
                    passwordStrengthForm.specificPasswordList =
                        data.prohibitedPasswordList || [];
                    if (passwordStrengthForm.passwordStrength === 4) {
                        passwordStrengthForm.minLength = data.minLength;
                        passwordStrengthForm.maxLength = data.maxLength;
                        passwordStrengthForm.charType = [];
                        if (data.requireNumber) {
                            passwordStrengthForm.charType.push("NUMBER");
                        }
                        if (data.requireLowerCase) {
                            passwordStrengthForm.charType.push("LOWER_CASE");
                        }
                        if (data.requireUpperCase) {
                            passwordStrengthForm.charType.push("UPPER_CASE");
                        }
                        if (data.requireSpecialChar) {
                            passwordStrengthForm.charType.push("SPECIAL_CHAR");
                        }
                        if (data.minCharTypeCount === passwordStrengthForm.charType.length) {
                            passwordStrengthForm.minCharTypeCount = 0;
                        }
                        else {
                            passwordStrengthForm.minCharTypeCount = data.minCharTypeCount;
                        }
                        passwordStrengthForm.prohibitUserInfo = data.prohibitUserInfo;
                        passwordStrengthForm.prohibitConsecutiveChar =
                            data.prohibitConsecutiveChar;
                        passwordStrengthForm.prohibitContainConsecutiveChar =
                            data.prohibitContainConsecutiveChar;
                        passwordStrengthForm.minConsecutiveCharLength =
                            data.minConsecutiveCharLength;
                        passwordStrengthForm.prohibitContainRepeatChar =
                            data.prohibitContainRepeatChar;
                        passwordStrengthForm.minRepeatCharLength = data.minRepeatCharLength;
                        passwordStrengthForm.prohibitSpecificPassword =
                            data.prohibitSpecificPassword;
                        passwordStrengthForm.prohibitSingleChar = data.prohibitSingleChar;
                    }
                    // 密码轮换策略
                    forceChangePasswordForm.enableForceChangePassword =
                        data.enableForceChangePassword;
                    forceChangePasswordForm.forcedCycle = data.forcedCycle;
                    forceChangePasswordForm.forcedCycleUnit = data.forcedCycleUnit;
                    forceChangePasswordForm.remindCycle = data.remindCycle;
                    forceChangePasswordForm.remindCycleUnit = data.remindCycleUnit;
                    // 策略应用主体
                    if (((_a = data.userIds) === null || _a === void 0 ? void 0 : _a.length) > 0) {
                        policyPrincipalForm.type = "USER";
                        policyPrincipalForm.id = data.userIds;
                    }
                    if (((_b = data.userGroupIds) === null || _b === void 0 ? void 0 : _b.length) > 0) {
                        policyPrincipalForm.type = "USER_GROUP";
                        policyPrincipalForm.id = data.userGroupIds;
                    }
                });
            })];
    });
}); };
/**
 * 提交更新密码策略表单
 */
var handleUpdatePasswordPolicyFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var hasError;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                hasError = false;
                updatePasswordPolicyForm.id = passwordPoliyId.value;
                // 基础信息
                return [4 /*yield*/, basicInfoFormRef.value.validate(function (errors) {
                        if (!errors) {
                            updatePasswordPolicyForm.name = basicInfoForm.name;
                            updatePasswordPolicyForm.desc = basicInfoForm.desc;
                        }
                        else {
                            hasError = true;
                        }
                    })];
            case 1:
                // 基础信息
                _a.sent();
                if (!(passwordPolicyName.value !== "默认策略")) return [3 /*break*/, 3];
                return [4 /*yield*/, policyPrincipalFormRef.value.validate(function (errors) {
                        var _a, _b;
                        if (!errors) {
                            if (policyPrincipalForm.type === "USER") {
                                updatePasswordPolicyForm.userIds.length = 0;
                                (_a = updatePasswordPolicyForm.userIds).push.apply(_a, policyPrincipalForm.id);
                            }
                            if (policyPrincipalForm.type === "USER_GROUP") {
                                updatePasswordPolicyForm.userGroupIds.length = 0;
                                (_b = updatePasswordPolicyForm.userGroupIds).push.apply(_b, policyPrincipalForm.id);
                            }
                        }
                        else {
                            hasError = true;
                        }
                    })];
            case 2:
                _a.sent();
                _a.label = 3;
            case 3:
                // 密码强度
                // 是否开启用户登录密码强度检查
                updatePasswordPolicyForm.enablePasswordDetection =
                    passwordStrengthForm.enablePasswordDetection;
                updatePasswordPolicyForm.passwordStrength =
                    passwordStrengthForm.passwordStrength;
                if (!(passwordStrengthForm.passwordStrength === 4)) return [3 /*break*/, 5];
                return [4 /*yield*/, passwordStrengthFormRef.value.validate(function (errors) {
                        if (!errors) {
                            handleSetPasswordStrength();
                        }
                        else {
                            hasError = true;
                        }
                    })];
            case 4:
                _a.sent();
                _a.label = 5;
            case 5:
                // 密码轮换策略
                // 是否开启强制修改密码
                updatePasswordPolicyForm.enableForceChangePassword =
                    forceChangePasswordForm.enableForceChangePassword;
                if (!forceChangePasswordForm.enableForceChangePassword) return [3 /*break*/, 7];
                return [4 /*yield*/, forceChangePasswordFormRef.value.validate(function (errors) {
                        if (!errors) {
                            // 强制修改密码周期
                            updatePasswordPolicyForm.forcedCycle =
                                forceChangePasswordForm.forcedCycle;
                            updatePasswordPolicyForm.forcedCycleUnit =
                                forceChangePasswordForm.forcedCycleUnit;
                            // 密码到期提醒周期
                            updatePasswordPolicyForm.remindCycle =
                                forceChangePasswordForm.remindCycle;
                            updatePasswordPolicyForm.remindCycleUnit =
                                forceChangePasswordForm.remindCycleUnit;
                        }
                        else {
                            hasError = true;
                        }
                    })];
            case 6:
                _a.sent();
                _a.label = 7;
            case 7:
                if (!hasError) {
                    updatePasswordPolicy(updatePasswordPolicyForm)
                        .then(function (result) {
                        handleApiSuccess(result, function () {
                            Notification.success("保存成功");
                            handleBack();
                        });
                    })
                        .catch(function (err) {
                        handleApiError(err, " 更新密码策略");
                    });
                }
                return [2 /*return*/];
        }
    });
}); };
var handleSetPasswordStrength = function () {
    updatePasswordPolicyForm.passwordStrength =
        passwordStrengthForm.passwordStrength;
    // 最小长度
    updatePasswordPolicyForm.minLength = passwordStrengthForm.minLength;
    // 最大长度
    updatePasswordPolicyForm.maxLength = passwordStrengthForm.maxLength;
    // 是否必须包含数字
    if (passwordStrengthForm.charType.includes("NUMBER")) {
        updatePasswordPolicyForm.requireNumber = true;
    }
    // 是否必须包含小写字母
    if (passwordStrengthForm.charType.includes("LOWER_CASE")) {
        updatePasswordPolicyForm.requireLowerCase = true;
    }
    // 是否必须包含大写字母
    if (passwordStrengthForm.charType.includes("UPPER_CASE")) {
        updatePasswordPolicyForm.requireUpperCase = true;
    }
    // 是否必须包含特殊字符
    if (passwordStrengthForm.charType.includes("SPECIAL_CHAR")) {
        updatePasswordPolicyForm.requireSpecialChar = true;
    }
    // 至少需要满足的字符类型数量
    if (passwordStrengthForm.minCharTypeCount === 0) {
        updatePasswordPolicyForm.minCharTypeCount =
            passwordStrengthForm.charType.length;
    }
    else {
        updatePasswordPolicyForm.minCharTypeCount =
            passwordStrengthForm.minCharTypeCount;
    }
    // 是否禁止包含用户信息
    updatePasswordPolicyForm.prohibitUserInfo =
        passwordStrengthForm.prohibitUserInfo;
    // 是否禁止全部为单一字符
    updatePasswordPolicyForm.prohibitSingleChar =
        passwordStrengthForm.prohibitSingleChar;
    // 是否禁止全部为连续字符
    updatePasswordPolicyForm.prohibitConsecutiveChar =
        passwordStrengthForm.prohibitConsecutiveChar;
    // 是否禁止包含连续字符
    updatePasswordPolicyForm.prohibitContainConsecutiveChar =
        passwordStrengthForm.prohibitContainConsecutiveChar;
    // 禁止包含的连续字符的最小长度
    updatePasswordPolicyForm.minConsecutiveCharLength =
        passwordStrengthForm.minConsecutiveCharLength;
    // 是否禁止包含连续重复字符
    updatePasswordPolicyForm.prohibitContainRepeatChar =
        passwordStrengthForm.prohibitContainRepeatChar;
    // 禁止包含的连续重复字符的最小长度
    updatePasswordPolicyForm.minRepeatCharLength =
        passwordStrengthForm.minRepeatCharLength;
    // 是否禁止使用特定密码
    updatePasswordPolicyForm.prohibitSpecificPassword =
        passwordStrengthForm.prohibitSpecificPassword;
    // 禁止使用的特定密码列表
    updatePasswordPolicyForm.prohibitedPasswordList =
        passwordStrengthForm.specificPasswordList;
};
/**
 * 重置更新密码策略表单
 */
var handleResetUpdatePasswordPolicyForm = function () {
    handleGetPasswordPolicyDetail(passwordPoliyId.value);
    passwordCheckerRef.value.setPassword("");
};
/**
 * 密码检查
 */
var passwordCheckerRef = ref(null);
var checkLoading = ref(false);
var checkRes = reactive({
    valid: false,
    errorMessage: undefined,
    ruleResults: undefined,
});
var handleCheckPassword = function (password) {
    handleSetPasswordStrength();
    delete updatePasswordPolicyForm.prohibitUserInfo;
    checkLoading.value = true;
    checkPassword({
        password: password,
        passwordPolicy: updatePasswordPolicyForm,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            checkRes.valid = data.valid;
            checkRes.errorMessage = data.errorMessage;
            if (data.ruleResults) {
                checkRes.ruleResults = data.ruleResults;
            }
            else {
                checkRes.ruleResults = [];
            }
            checkLoading.value = false;
        });
    })
        .catch(function (err) {
        handleApiError(err, "密码检查");
        checkLoading.value = false;
    });
};
var handleResetCheckRes = function () {
    checkRes.valid = false;
    checkRes.errorMessage = undefined;
    checkRes.ruleResults = undefined;
};
export default defineComponent({
    setup: function () {
        var _this = this;
        var passwordPolicyId = getQueryString("id");
        passwordPoliyId.value = passwordPolicyId;
        onMounted(function () { return __awaiter(_this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0: return [4 /*yield*/, handleGetPasswordPolicyDetail(passwordPolicyId)];
                    case 1:
                        _a.sent();
                        if (policyPrincipalForm.type === "USER") {
                            handleGetAllUsers();
                        }
                        if (policyPrincipalForm.type === "USER_GROUP") {
                            handleGetAllUserGroups();
                        }
                        return [2 /*return*/];
                }
            });
        }); });
        return {
            handleBack: handleBack,
            passwordPolicyName: passwordPolicyName,
            basicInfoForm: basicInfoForm,
            basicInfoFormRef: basicInfoFormRef,
            basicInfoFormRules: basicInfoFormRules,
            passwordStrengthForm: passwordStrengthForm,
            passwordStrengthFormRef: passwordStrengthFormRef,
            passwordStrengthFormRules: passwordStrengthFormRules,
            forceChangePasswordForm: forceChangePasswordForm,
            forceChangePasswordFormRef: forceChangePasswordFormRef,
            forceChangePasswordFormRules: forceChangePasswordFormRules,
            policyPrincipalForm: policyPrincipalForm,
            policyPrincipalFormRef: policyPrincipalFormRef,
            policyPrincipalFormRules: policyPrincipalFormRules,
            userList: userList,
            userSearchKeyword: userSearchKeyword,
            handleGetAllUsers: handleGetAllUsers,
            userGroupList: userGroupList,
            userGroupSearchKeyword: userGroupSearchKeyword,
            handleGetAllUserGroups: handleGetAllUserGroups,
            principalSelectChange: principalSelectChange,
            passwordStrengthLabel: passwordStrengthLabel,
            specificPasswordListEditModalVisible: specificPasswordListEditModalVisible,
            handleAddSpecificPasswordInputItem: handleAddSpecificPasswordInputItem,
            handleDeleteSpecificPasswordInputItem: handleDeleteSpecificPasswordInputItem,
            handleUpdatePasswordPolicyFormSubmit: handleUpdatePasswordPolicyFormSubmit,
            handleResetUpdatePasswordPolicyForm: handleResetUpdatePasswordPolicyForm,
            passwordCheckerRef: passwordCheckerRef,
            checkLoading: checkLoading,
            checkRes: checkRes,
            handleCheckPassword: handleCheckPassword,
        };
    },
});
