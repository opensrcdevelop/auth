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
import { checkPassword, createPasswordPolicy } from "@/api/setting";
import { searchUser } from "@/api/user";
import { getUserGroupList } from "@/api/userGroup";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { computed, defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    handleResetCreatePasswordPolicyForm();
    router.back();
};
var createPasswordPolicyForm = reactive({
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
var userListPagination = {
    current: 1,
    total: 0,
};
/**
 * 获取用户列表
 */
var handleGetUserList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    searchUser(userSearchKeyword.value, {
        page: page,
        size: size,
    }).then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                userList.length = 0;
                userList.push.apply(userList, data.list);
            }
            else {
                userList.push.apply(userList, data.list);
            }
            userListPagination.current = data.current;
            userListPagination.total = data.total;
        });
    });
};
/**
 * 搜索用户
 */
var handleSearchUser = function () {
    handleGetUserList(1);
};
/**
 * 加载更多用户
 */
var loadMoreUserLoading = false;
var loadMoreUser = function () {
    if (loadMoreUserLoading)
        return;
    if (userList.length < userListPagination.total) {
        loadMoreUserLoading = true;
        userListPagination.current++;
        handleGetUserList(userListPagination.current);
        loadMoreUserLoading = false;
    }
};
/** 用户组列表 */
var userGroupList = reactive([]);
var userGroupSearchKeyword = ref("");
var userGroupListPagination = {
    current: 1,
    total: 0,
};
/**
 * 获取用户组列表
 */
var handleGetUserGroupList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserGroupList({
        page: page,
        size: size,
        keyword: userGroupSearchKeyword.value,
    }).then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                userGroupList.length = 0;
                userGroupList.push.apply(userGroupList, data.list);
            }
            else {
                userGroupList.push.apply(userGroupList, data.list);
            }
            userGroupListPagination.current = data.current;
            userGroupListPagination.total = data.total;
        });
    });
};
/**
 * 搜索用户组
 */
var handleSearchUserGroup = function () {
    handleGetUserGroupList(1);
};
/**
 * 加载更多用户组
 */
var loadMoreUserGroupLoading = false;
var loadMoreUserGroup = function () {
    if (loadMoreUserGroupLoading)
        return;
    if (userGroupList.length < userGroupListPagination.total) {
        loadMoreUserGroupLoading = true;
        userGroupListPagination.current++;
        handleGetUserGroupList(userGroupListPagination.current);
        loadMoreUserGroupLoading = false;
    }
};
/**
 * 选择主体类型变化
 */
var principalSelectChange = function (value) {
    policyPrincipalForm.id.length = 0;
    if (value === "USER") {
        handleGetUserList(1);
    }
    if (value === "USER_GROUP") {
        handleGetUserGroupList(1);
    }
};
/**
 * 提交创建密码策略表单
 */
var handleCreatePasswordPolicyFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var hasError;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                hasError = false;
                // 基础信息
                return [4 /*yield*/, basicInfoFormRef.value.validate(function (errors) {
                        if (!errors) {
                            createPasswordPolicyForm.name = basicInfoForm.name;
                            createPasswordPolicyForm.desc = basicInfoForm.desc;
                        }
                        else {
                            hasError = true;
                        }
                    })];
            case 1:
                // 基础信息
                _a.sent();
                // 策略应用主体
                return [4 /*yield*/, policyPrincipalFormRef.value.validate(function (errors) {
                        var _a, _b;
                        if (!errors) {
                            if (policyPrincipalForm.type === "USER") {
                                createPasswordPolicyForm.userIds.length = 0;
                                (_a = createPasswordPolicyForm.userIds).push.apply(_a, policyPrincipalForm.id);
                            }
                            if (policyPrincipalForm.type === "USER_GROUP") {
                                createPasswordPolicyForm.userGroupIds.length = 0;
                                (_b = createPasswordPolicyForm.userGroupIds).push.apply(_b, policyPrincipalForm.id);
                            }
                        }
                        else {
                            hasError = true;
                        }
                    })];
            case 2:
                // 策略应用主体
                _a.sent();
                // 密码强度
                // 是否开启用户登录密码强度检查
                createPasswordPolicyForm.enablePasswordDetection =
                    passwordStrengthForm.enablePasswordDetection;
                createPasswordPolicyForm.passwordStrength =
                    passwordStrengthForm.passwordStrength;
                if (!(passwordStrengthForm.passwordStrength === 4)) return [3 /*break*/, 4];
                return [4 /*yield*/, passwordStrengthFormRef.value.validate(function (errors) {
                        handleSetPasswordStrength();
                    })];
            case 3:
                _a.sent();
                _a.label = 4;
            case 4:
                // 密码轮换策略
                // 是否开启强制修改密码
                createPasswordPolicyForm.enableForceChangePassword =
                    forceChangePasswordForm.enableForceChangePassword;
                if (!forceChangePasswordForm.enableForceChangePassword) return [3 /*break*/, 6];
                return [4 /*yield*/, forceChangePasswordFormRef.value.validate(function (errors) {
                        if (!errors) {
                            // 强制修改密码周期
                            createPasswordPolicyForm.forcedCycle =
                                forceChangePasswordForm.forcedCycle;
                            createPasswordPolicyForm.remindCycle =
                                forceChangePasswordForm.remindCycle;
                            // 密码到期提醒周期
                            createPasswordPolicyForm.remindCycle =
                                forceChangePasswordForm.remindCycle;
                            createPasswordPolicyForm.remindCycleUnit =
                                forceChangePasswordForm.remindCycleUnit;
                        }
                        else {
                            hasError = true;
                        }
                    })];
            case 5:
                _a.sent();
                _a.label = 6;
            case 6:
                if (!hasError) {
                    createPasswordPolicy(createPasswordPolicyForm)
                        .then(function (result) {
                        handleApiSuccess(result, function () {
                            Notification.success("创建成功");
                            handleResetCreatePasswordPolicyForm();
                        });
                    })
                        .catch(function (err) {
                        handleApiError(err, "创建密码策略");
                    });
                }
                return [2 /*return*/];
        }
    });
}); };
var handleSetPasswordStrength = function () {
    createPasswordPolicyForm.passwordStrength =
        passwordStrengthForm.passwordStrength;
    // 最小长度
    createPasswordPolicyForm.minLength = passwordStrengthForm.minLength;
    // 最大长度
    createPasswordPolicyForm.maxLength = passwordStrengthForm.maxLength;
    // 是否必须包含数字
    if (passwordStrengthForm.charType.includes("NUMBER")) {
        createPasswordPolicyForm.requireNumber = true;
    }
    // 是否必须包含小写字母
    if (passwordStrengthForm.charType.includes("LOWER_CASE")) {
        createPasswordPolicyForm.requireLowerCase = true;
    }
    // 是否必须包含大写字母
    if (passwordStrengthForm.charType.includes("UPPER_CASE")) {
        createPasswordPolicyForm.requireUpperCase = true;
    }
    // 是否必须包含特殊字符
    if (passwordStrengthForm.charType.includes("SPECIAL_CHAR")) {
        createPasswordPolicyForm.requireSpecialChar = true;
    }
    // 至少需要满足的字符类型数量
    if (passwordStrengthForm.minCharTypeCount === 0) {
        createPasswordPolicyForm.minCharTypeCount =
            passwordStrengthForm.charType.length;
    }
    else {
        createPasswordPolicyForm.minCharTypeCount =
            passwordStrengthForm.minCharTypeCount;
    }
    // 是否禁止包含用户信息
    createPasswordPolicyForm.prohibitUserInfo =
        passwordStrengthForm.prohibitUserInfo;
    // 是否禁止全部为单一字符
    createPasswordPolicyForm.prohibitSingleChar =
        passwordStrengthForm.prohibitSingleChar;
    // 是否禁止全部为连续字符
    createPasswordPolicyForm.prohibitConsecutiveChar =
        passwordStrengthForm.prohibitConsecutiveChar;
    // 是否禁止包含连续字符
    createPasswordPolicyForm.prohibitContainConsecutiveChar =
        passwordStrengthForm.prohibitContainConsecutiveChar;
    // 禁止包含的连续字符的最小长度
    createPasswordPolicyForm.minConsecutiveCharLength =
        passwordStrengthForm.minConsecutiveCharLength;
    // 是否禁止包含连续重复字符
    createPasswordPolicyForm.prohibitContainRepeatChar =
        passwordStrengthForm.prohibitContainRepeatChar;
    // 禁止包含的连续重复字符的最小长度
    createPasswordPolicyForm.minRepeatCharLength =
        passwordStrengthForm.minRepeatCharLength;
    // 是否禁止使用特定密码
    createPasswordPolicyForm.prohibitSpecificPassword =
        passwordStrengthForm.prohibitSpecificPassword;
    // 禁止使用的特定密码列表
    createPasswordPolicyForm.prohibitedPasswordList =
        passwordStrengthForm.specificPasswordList;
};
/**
 * 重置创建密码策略表单
 */
var handleResetCreatePasswordPolicyForm = function () {
    basicInfoFormRef.value.resetFields();
    policyPrincipalFormRef.value.resetFields();
    passwordStrengthFormRef.value.resetFields();
    forceChangePasswordFormRef.value.resetFields();
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
    delete createPasswordPolicyForm.prohibitUserInfo;
    checkLoading.value = true;
    checkPassword({
        password: password,
        passwordPolicy: createPasswordPolicyForm,
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
export default defineComponent({
    setup: function () {
        onMounted(function () {
            handleGetUserList(1);
        });
        return {
            handleBack: handleBack,
            basicInfoForm: basicInfoForm,
            basicInfoFormRef: basicInfoFormRef,
            basicInfoFormRules: basicInfoFormRules,
            passwordStrengthForm: passwordStrengthForm,
            passwordStrengthFormRef: passwordStrengthFormRef,
            passwordStrengthFormRules: passwordStrengthFormRules,
            passwordStrengthLabel: passwordStrengthLabel,
            specificPasswordListEditModalVisible: specificPasswordListEditModalVisible,
            handleAddSpecificPasswordInputItem: handleAddSpecificPasswordInputItem,
            handleDeleteSpecificPasswordInputItem: handleDeleteSpecificPasswordInputItem,
            forceChangePasswordForm: forceChangePasswordForm,
            forceChangePasswordFormRef: forceChangePasswordFormRef,
            forceChangePasswordFormRules: forceChangePasswordFormRules,
            policyPrincipalForm: policyPrincipalForm,
            policyPrincipalFormRef: policyPrincipalFormRef,
            policyPrincipalFormRules: policyPrincipalFormRules,
            userList: userList,
            userSearchKeyword: userSearchKeyword,
            handleSearchUser: handleSearchUser,
            loadMoreUser: loadMoreUser,
            userGroupList: userGroupList,
            userGroupSearchKeyword: userGroupSearchKeyword,
            handleSearchUserGroup: handleSearchUserGroup,
            loadMoreUserGroup: loadMoreUserGroup,
            principalSelectChange: principalSelectChange,
            handleCreatePasswordPolicyFormSubmit: handleCreatePasswordPolicyFormSubmit,
            handleResetCreatePasswordPolicyForm: handleResetCreatePasswordPolicyForm,
            passwordCheckerRef: passwordCheckerRef,
            checkRes: checkRes,
            checkLoading: checkLoading,
            handleCheckPassword: handleCheckPassword,
        };
    },
});
