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
import { computed, defineComponent, h, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { clearAuthorizedTokens, clearAuthorizedTokensByLoginId, getUserAttrs, getUserDetail, getUserLoginLogs, getUserPermissions, rebindMfaDevice, updateUser, } from "@/api/user";
import { generateRandomString, getQueryString, handleApiError, handleApiSuccess, } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { addRoleMapping, getRoleList, removeRoleMapping } from "@/api/role";
import { addUserGroupMapping, getUserGroupList, removeUserGroupMapping, } from "@/api/userGroup";
import { cancelAuthorization } from "@/api/permission";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import IconSearch from "@arco-design/web-vue/es/icon/icon-search";
import { getEnabledDictData } from "@/api/dict";
import { usePagination } from "@/hooks/usePagination";
import { checkPasswordWithoutPolicy } from "@/api/setting";
import { IconFilter } from "@arco-design/web-vue/es/icon";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("user_info");
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
    handleTabInit(tabKey);
};
var handleTabInit = function (tabKey, id) {
    if (id === void 0) { id = userId.value; }
    switch (tabKey) {
        case "user_info":
            handleGetUserDetail(id);
            handleGetUserExtAttrs();
            break;
        case "user_belong":
            handleGetUserDetail(id);
        case "permission_management":
            handleGetUserDetail(id);
            handleGetUserPermissions(id);
            break;
        case "login_logs":
            handleGetUserDetail(id);
            handleGetUserLoginLogs(id);
            break;
    }
};
var userId = ref("");
var username = ref("");
/** 账号信息 */
var accountInfoForm = reactive({
    userId: "",
    createTime: "",
    lastLoginTime: "",
    lastLoginIp: "",
    lastLoginDeviceType: "",
    lastLoginDeviceOs: "",
});
/** 个人信息 */
var userInfoForm = reactive({
    userId: "",
    username: "",
    phoneNumber: "",
    emailAddress: "",
});
var userInfoFormRef = ref();
/** 所有用户扩展属性 */
var allUserExtAttrs = reactive([]);
var userAttrs = reactive([]);
/** 字典数据值 */
var allDictDatas = reactive({});
/** 用户角色 */
var userRoles = reactive([]);
/** 所属用户组 */
var userGroups = reactive([]);
/** 账户禁用状态 */
var accountLocked = ref(false);
/** MFA（多因素认证）状态 */
var enableMfa = ref(false);
/** 控制台访问状态 */
var consoleAccess = ref(false);
/**
 * 获取用户详情
 *
 * @param id 用户ID
 */
var handleGetUserDetail = function (id) {
    getUserDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userId.value = data.id;
            username.value = data.username;
            accountInfoForm.userId = data.id;
            accountInfoForm.createTime = data.createTime;
            accountInfoForm.lastLoginTime = data.lastLoginTime;
            accountInfoForm.lastLoginIp = data.lastLoginIp;
            accountInfoForm.lastLoginDeviceType = data.lastLoginDeviceType;
            accountInfoForm.lastLoginDeviceOs = data.lastLoginDeviceOs;
            userInfoForm.userId = data.id;
            userInfoForm.username = data.username;
            userInfoForm.phoneNumber = data.phoneNumber;
            userInfoForm.emailAddress = data.emailAddress;
            userAttrs.length = 0;
            userAttrs.push.apply(userAttrs, data.attributes);
            userRoles.length = 0;
            userRoles.push.apply(userRoles, data.roles);
            userGroups.length = 0;
            userGroups.push.apply(userGroups, data.userGroups);
            resetPwdForm.rawEmail = data.emailAddress;
            resetPwdForm.userId = data.id;
            accountLocked.value = data.locked;
            enableMfa.value = data.enableMfa;
            consoleAccess.value = data.consoleAccess;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户详情");
    });
};
/** 用户权限 */
var permissionsPagination;
var permissions = reactive([]);
var authorizeSearchKeywords = reactive({
    // 资源组名称检索关键字
    resourceGroupName: undefined,
    // 资源名称检索关键字
    resourceName: undefined,
    // 权限名称检索关键字
    permissionName: undefined,
    // 权限标识检索关键字
    permissionCode: undefined,
});
/** 过滤标记 */
var authorizeFilteredFlags = reactive({
    resourceGroupName: false,
    resourceName: false,
    permissionName: false,
    permissionCode: false,
});
// 资源组名称过滤
var resourceGroupNameFilter = {
    slotName: "resource-group-name-filter",
    icon: function () {
        return authorizeFilteredFlags.resourceGroupName ? h(IconFilter) : h(IconSearch);
    },
};
// 资源名称过滤
var resourceNameFilter = {
    slotName: "resource-name-filter",
    icon: function () {
        return authorizeFilteredFlags.resourceName ? h(IconFilter) : h(IconSearch);
    },
};
// 权限名称过滤
var permissionNameFilter = {
    slotName: "permission-name-filter",
    icon: function () {
        return authorizeFilteredFlags.permissionName ? h(IconFilter) : h(IconSearch);
    },
};
// 资源标识过滤
var permissionCodeFilter = {
    slotName: "permission-code-filter",
    icon: function () {
        return authorizeFilteredFlags.permissionCode ? h(IconFilter) : h(IconSearch);
    },
};
/**
 * 获取用户权限
 */
var handleGetUserPermissions = function (id, page, size) {
    if (id === void 0) { id = userId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserPermissions(id, {
        page: page,
        size: size,
        resourceGroupNameSearchKeyword: authorizeSearchKeywords.resourceGroupName,
        resourceNameSearchKeyword: authorizeSearchKeywords.resourceName,
        permissionNameSearchKeyword: authorizeSearchKeywords.permissionName,
        permissionCodeSearchKeyword: authorizeSearchKeywords.permissionCode,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            permissions.length = 0;
            permissions.push.apply(permissions, data.list);
            permissionsPagination.updatePagination(data.current, data.total, data.size);
            // 设置过滤标记
            if (authorizeSearchKeywords.resourceGroupName) {
                authorizeFilteredFlags.resourceGroupName = true;
            }
            if (authorizeSearchKeywords.resourceName) {
                authorizeFilteredFlags.resourceName = true;
            }
            if (authorizeSearchKeywords.permissionName) {
                authorizeFilteredFlags.permissionName = true;
            }
            if (authorizeSearchKeywords.permissionCode) {
                authorizeFilteredFlags.permissionCode = true;
            }
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户权限");
    });
};
/**
 * 重置过滤
 */
var handleResetPermissionFilter = function (keyword) {
    authorizeSearchKeywords[keyword] = undefined;
    authorizeFilteredFlags[keyword] = false;
    handleGetUserPermissions();
};
/**
 * 获取全部用户扩展属性
 */
var handleGetUserExtAttrs = function () {
    getUserAttrs({
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            allUserExtAttrs.length = 0;
            data.list.forEach(function (item) {
                if (item.extFlg) {
                    allUserExtAttrs.push(item);
                }
                if (item.dataType === "DICT" && item.dictId) {
                    allDictDatas[item.key] = [];
                    handleGetEnabledDictData(item.key, item.dictId);
                }
            });
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户扩展属性");
    });
};
/**
 * 获取启用的字典数据
 */
var handleGetEnabledDictData = function (attrKey, dictId) { return __awaiter(void 0, void 0, void 0, function () {
    var result, err_1;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0:
                _a.trys.push([0, 2, , 3]);
                return [4 /*yield*/, getEnabledDictData(dictId)];
            case 1:
                result = _a.sent();
                handleApiSuccess(result, function (data) {
                    var _a;
                    allDictDatas[attrKey].length = 0;
                    (_a = allDictDatas[attrKey]).push.apply(_a, data);
                });
                return [3 /*break*/, 3];
            case 2:
                err_1 = _a.sent();
                handleApiError(err_1, "获取启用的字典数据");
                return [3 /*break*/, 3];
            case 3: return [2 /*return*/];
        }
    });
}); };
/**
 * 用户扩展属性值
 */
var userAttrValues = computed(function () {
    var userAttrValues = [];
    allUserExtAttrs.map(function (item) {
        var userAttr = userAttrs.find(function (attr) { return attr.key === item.key; });
        if (userAttr) {
            userAttrValues.push(userAttr.value);
        }
        else {
            userAttrValues.push(null);
        }
    });
    return reactive(userAttrValues);
});
/**
 * 设置账号状态
 *
 * @param locked 账号状态
 */
var handleSetAccountStatus = function (locked) {
    updateUser({
        userId: userId.value,
        locked: locked,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success(locked ? "禁用成功" : "启用成功");
            handleGetUserDetail(userId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新用户信息");
    });
};
/**
 * 设置 MFA 状态
 *
 * @param enableMfa MFA 状态
 */
var handleSetMfaStatus = function (enableMfa) {
    updateUser({
        userId: userId.value,
        enableMfa: enableMfa,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success(enableMfa ? "启用 MFA 成功" : "关闭 MFA 成功");
            handleGetUserDetail(userId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新用户信息");
    });
};
/**
 * 重新绑定 MFA 设备
 */
var handleRebindMfaDevice = function () {
    rebindMfaDevice(userId.value)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("重新绑定 MFA 设备成功");
        });
    })
        .catch(function (err) {
        handleApiError(err, "重新绑定 MFA 设备");
    });
};
/**
 * 设置控制台访问状态
 *
 * @param consoleAccess 控制台访问状态
 */
var handleSetConsoleAccessStatus = function (consoleAccess) {
    updateUser({
        userId: userId.value,
        consoleAccess: consoleAccess,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success(consoleAccess ? "开启控制台访问成功" : "关闭控制台访问成功");
            handleGetUserDetail(userId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新用户信息");
    });
};
/**
 * 清除授权
 */
var handleClearAuthorizedTokens = function () {
    clearAuthorizedTokens(userId.value)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("清除授权的 Token 成功");
        });
    })
        .catch(function (err) {
        handleApiError(err, "清除授权的 Token");
    });
};
/**
 * 提交个人信息表单
 *
 * @param formData 个人信息表单
 */
var handleUserInfoFormSubmit = function (formData) {
    updateUser(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetUserDetail(userId.value);
            handleGetUserExtAttrs();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新用户信息");
    });
};
/**
 * 提交用户扩展属性表单
 */
var handleUserAttrsSubmit = function () {
    var userAttrs = [];
    allUserExtAttrs.forEach(function (item, index) {
        userAttrs.push({
            attrId: item.id,
            attrValue: userAttrValues.value[index],
        });
    });
    updateUser({
        userId: userId.value,
        attributes: userAttrs,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetUserDetail(userId.value);
            handleGetUserExtAttrs();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新用户信息");
    });
};
/**
 * 重置个人信息表单
 */
var handleResetUserInfoForm = function () {
    userInfoFormRef.value.resetFields();
    handleGetUserDetail(userId.value);
    handleGetUserExtAttrs();
};
/**
 * 重置用户扩展属性
 */
var handleResetUserAttrs = function () {
    handleGetUserDetail(userId.value);
    handleGetUserExtAttrs();
};
/**
 * 撤销用户角色
 *
 * @param role 角色
 */
var handleRemoveUserRole = function (role) {
    Modal.warning({
        title: "\u786E\u5B9A\u64A4\u9500\u89D2\u8272\u300C".concat(role.name, "\u300D\u5417\uFF1F"),
        content: "\u64A4\u9500\u540E\u8BE5\u7528\u6237\u5C06\u5931\u53BB\u300C".concat(role.name, "\u300D\u6240\u62E5\u6709\u7684\u6743\u9650\u3002"),
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeRoleMapping({
                userIds: [userId.value],
                roleIds: [role.id],
            })
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("撤销成功");
                    handleGetUserDetail(userId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "撤销角色");
            });
        },
    });
};
/**
 * 移除用户组
 *
 * @param group 用户组
 */
var handleRemoveUserGroup = function (group) {
    Modal.warning({
        title: "\u786E\u5B9A\u79FB\u9664\u7528\u6237\u7EC4\u300C".concat(group.name, "\u300D\u5417\uFF1F"),
        content: "此操作不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeUserGroupMapping({
                userIds: [userId.value],
                userGroupIds: [group.id],
            })
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("移除成功");
                    handleGetUserDetail(userId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "移除用户组");
            });
        },
    });
};
var roleList = reactive([]);
var roleListPagination = {
    current: 1,
    total: 0,
};
var roleSearchKeyword = ref("");
var addUserRoleModalVisible = ref(false);
var addUserRoleFormRef = ref();
var addUserRoleForm = reactive({
    roleIds: [],
});
var addUserRoleFormRules = {
    roleIds: [
        {
            required: true,
            message: "至少选择一项",
        },
    ],
};
/**
 * 获取角色列表
 */
var handleGetRoleList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getRoleList({
        page: page,
        size: size,
        keyword: roleSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                roleList.length = 0;
                roleList.push.apply(roleList, data.list);
            }
            else {
                roleList.push.apply(roleList, data.list);
            }
            roleListPagination.current = data.current;
            roleListPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取角色列表");
    });
};
/**
 * 搜索角色
 */
var handleSearchRole = function () {
    handleGetRoleList(1);
};
/**
 * 加载更多角色
 */
var loadMoreRoleLoading = false;
var loadMoreRole = function () {
    if (loadMoreRoleLoading)
        return;
    if (roleList.length < roleListPagination.total) {
        loadMoreRoleLoading = true;
        roleListPagination.current++;
        handleGetRoleList(roleListPagination.current);
        loadMoreRoleLoading = false;
    }
};
/**
 * 打开添加用户角色对话框
 */
var handleOpenAddUserRoleModal = function () {
    handleGetRoleList();
    addUserRoleModalVisible.value = true;
};
/**
 * 关闭添加用户角色对话框
 */
var handleCloseAddUserRoleModal = function () {
    addUserRoleFormRef.value.resetFields();
    addUserRoleModalVisible.value = false;
};
/**
 * 提交添加用户角色表单
 *
 * @param formData 添加用户角色表单
 */
var addUserRoleFormSubmitLoading = ref(false);
var handleAddUserRoleFormSubmit = function (formData) {
    addUserRoleFormSubmitLoading.value = true;
    addRoleMapping({
        userIds: [userId.value],
        roleIds: formData.roleIds,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("添加成功");
            handleCloseAddUserRoleModal();
            handleGetUserDetail(userId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "添加角色");
    })
        .finally(function () {
        addUserRoleFormSubmitLoading.value = false;
    });
};
/** 用户组列表 */
var userGroupList = reactive([]);
var userGroupListPagination = {
    current: 1,
    total: 0,
};
var userGroupSearchKeyword = ref("");
var addUserGroupModalVisible = ref(false);
var addUserGroupFormRef = ref();
var addUserGroupForm = reactive({
    userGroupIds: [],
});
var addUserGroupFormRules = {
    userGroupIds: [
        {
            required: true,
            message: "至少选择一项",
        },
    ],
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
 * 打开添加用户组对话框
 */
var handleOpenAddUserGroupModal = function () {
    handleGetUserGroupList();
    addUserGroupModalVisible.value = true;
};
/**
 * 关闭添加用户组对话框
 */
var handleCloseAddUserGroupModal = function () {
    addUserGroupFormRef.value.resetFields();
    addUserGroupModalVisible.value = false;
};
/**
 * 提交添加用户组表单
 *
 * @param formData 用户组表单
 */
var addUserGroupFormSubmitLoading = ref(false);
var handleAddUserGroupFormSubmit = function (formData) {
    addUserGroupFormSubmitLoading.value = true;
    addUserGroupMapping({
        userIds: [userId.value],
        userGroupIds: formData.userGroupIds,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("添加成功");
            handleCloseAddUserGroupModal();
            handleGetUserDetail(userId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "添加用户组");
    })
        .finally(function () {
        addUserGroupFormSubmitLoading.value = false;
    });
};
/** 重置密码对话框 */
var resetPwdModalVisible = ref(false);
var resetPwdForm = reactive({
    userId: "",
    password: "",
    rawEmail: "",
    emailAddress: "",
    needChangePwd: true,
    sendEmail: true,
});
var resetPwdFormRules = {
    password: [
        {
            required: true,
            message: "密码未填写",
        },
    ],
    emailAddress: [
        {
            validator: function (value, cb) {
                if (resetPwdForm.sendEmail && !resetPwdForm.rawEmail && !value) {
                    cb("邮箱未填写");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
var resetPwdFormRef = ref();
var resetPwdFormSubmitLoading = ref(false);
/** 打开重置密码对话框 */
var handleOpenResetPwdModal = function () {
    resetPwdModalVisible.value = true;
};
/** 关闭重置密码对话框 */
var handleCloseResetPwdModal = function () {
    resetPwdFormRef.value.resetFields();
    passwordCheckerRef.value.setPassword("");
    resetPwdModalVisible.value = false;
};
/**
 * 生成随机密码
 */
var handleGeneratePassword = function () {
    passwordCheckerRef.value.setPassword(generateRandomString(12));
};
/**
 * 提交重置密码表单
 */
var handleResetPwdFormSubmit = function () {
    if (!checkPasswordRes.valid) {
        return;
    }
    if (resetPwdForm.rawEmail && !resetPwdForm.emailAddress) {
        resetPwdForm.emailAddress = resetPwdForm.rawEmail;
    }
    else {
        resetPwdForm.emailAddress = undefined;
    }
    resetPwdFormSubmitLoading.value = true;
    updateUser({
        userId: resetPwdForm.userId,
        password: resetPwdForm.password,
        emailAddress: resetPwdForm.emailAddress,
        needChangePwd: resetPwdForm.needChangePwd,
        sendEmail: resetPwdForm.sendEmail,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("重置密码成功");
            handleCloseResetPwdModal();
        });
    })
        .catch(function (err) {
        handleApiError(err, "重置密码");
    })
        .finally(function () {
        resetPwdFormSubmitLoading.value = false;
    });
};
/**
 * 密码检查
 */
var passwordCheckerRef = ref(null);
var checkPasswordLoading = ref(false);
var checkPasswordRes = reactive({
    valid: false,
    errorMessage: undefined,
    ruleResults: undefined,
});
var handleCheckPassword = function (password) {
    checkPasswordLoading.value = true;
    resetPwdForm.password = password;
    checkPasswordWithoutPolicy({
        identity: resetPwdForm.userId,
        password: password,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            checkPasswordRes.valid = data.valid;
            checkPasswordRes.errorMessage = data.errorMessage;
            if (data.ruleResults) {
                checkPasswordRes.ruleResults = data.ruleResults;
            }
            else {
                checkPasswordRes.ruleResults = [];
            }
            checkPasswordLoading.value = false;
        });
    })
        .catch(function (err) {
        handleApiError(err, "密码检查");
        checkPasswordLoading.value = false;
    });
};
/**
 * 取消授权
 */
var handleCancelAuthorization = function (permission) {
    Modal.warning({
        title: "\u786E\u5B9A\u53D6\u6D88\u5BF9\u6743\u9650\u300C".concat(permission.permissionName, "\u300D\u7684\u6388\u6743\u5417\uFF1F"),
        content: "此操作不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            cancelAuthorization(permission.permissionId, userId.value)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("取消授权成功");
                    handleGetUserPermissions();
                });
            })
                .catch(function (err) {
                handleApiError(err, "取消授权");
            });
        },
    });
};
/**
 * 跳转角色详情
 *
 * @param role 角色
 */
var handleToRoleDetail = function (role) {
    router.push({
        path: "/role/detail",
        query: {
            id: role.id,
        },
    });
};
/**
 * 跳转用户组详情
 *
 * @param userGroup 用户组
 */
var hantoToUserGroupDetail = function (userGroup) {
    router.push({
        path: "/user/group/detail",
        query: {
            id: userGroup.id,
        },
    });
};
/** 授权对话框 */
var authorizeVisible = ref(false);
/**
 * 授权
 */
var handleAuthorize = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.authorizeOptions.principal = username.value;
    globalVariables.authorizeOptions.principalId = userId.value;
    globalVariables.authorizeOptions.principalType = "USER";
    authorizeVisible.value = true;
};
/**
 * 跳转资源组详情
 */
var handleToResourceGroupDetail = function (id) {
    router.push({
        path: "/resource/group/detail",
        query: {
            id: id,
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
        },
    });
};
/** 登录日志 */
var loginLogs = reactive([]);
var loginLogsPagination = reactive({
    total: 0,
    current: 1,
    pageSize: 15,
    showPageSize: true,
    showTotal: true,
    pageSizeOptions: [15, 25, 50],
});
/**
 * 获取用户登录日志
 */
var handleGetUserLoginLogs = function (id, page, size) {
    if (id === void 0) { id = userId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserLoginLogs(id, {
        page: page,
        size: size,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            loginLogs.length = 0;
            loginLogs.push.apply(loginLogs, data.list);
            loginLogsPagination.total = data.total;
            loginLogsPagination.current = data.current;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户登录日志");
    });
};
/**
 * 用户登录日志页数变化
 *
 * @param page 页数
 */
var handleLoginLogsPageChange = function (page) {
    loginLogsPagination.current = page;
    handleGetUserLoginLogs(userId.value, page, permissionsPagination.pageSize);
};
/**
 * 用户登录日志分页大小变化
 *
 * @param page 分页大小
 */
var handleLoginLogsPageSizeChange = function (size) {
    loginLogsPagination.pageSize = size;
    handleGetUserLoginLogs(userId.value, 1, size);
};
/**
 * 清除登录 ID 关联的 Token
 */
var handleClearAuthorizedTokensByLoginId = function (loginLog) {
    clearAuthorizedTokensByLoginId(loginLog.loginId)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("清除本次登录授权的 Token 成功");
        });
    })
        .catch(function (err) {
        handleApiError(err, "清除本次登录授权的 Token");
    });
};
export default defineComponent({
    setup: function () {
        var userId = getQueryString("id");
        permissionsPagination = usePagination("".concat(userId, "_userPermissions"), function (_a) {
            var page = _a.page, size = _a.size;
            if (getQueryString("active_tab") === "permission_management") {
                handleGetUserPermissions(userId, page, size);
            }
        });
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "user_info";
            handleTabInit(activeTab.value, userId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            userId: userId,
            username: username,
            userInfoForm: userInfoForm,
            accountInfoForm: accountInfoForm,
            allUserExtAttrs: allUserExtAttrs,
            userAttrValues: userAttrValues,
            handleUserInfoFormSubmit: handleUserInfoFormSubmit,
            handleUserAttrsSubmit: handleUserAttrsSubmit,
            userInfoFormRef: userInfoFormRef,
            handleResetUserInfoForm: handleResetUserInfoForm,
            handleResetUserAttrs: handleResetUserAttrs,
            userRoles: userRoles,
            userGroups: userGroups,
            permissions: permissions,
            handleRemoveUserRole: handleRemoveUserRole,
            handleRemoveUserGroup: handleRemoveUserGroup,
            addUserRoleModalVisible: addUserRoleModalVisible,
            handleOpenAddUserRoleModal: handleOpenAddUserRoleModal,
            addUserRoleForm: addUserRoleForm,
            roleList: roleList,
            roleSearchKeyword: roleSearchKeyword,
            loadMoreRole: loadMoreRole,
            handleSearchRole: handleSearchRole,
            addUserRoleFormRules: addUserRoleFormRules,
            addUserRoleFormRef: addUserRoleFormRef,
            handleCloseAddUserRoleModal: handleCloseAddUserRoleModal,
            addUserRoleFormSubmitLoading: addUserRoleFormSubmitLoading,
            handleAddUserRoleFormSubmit: handleAddUserRoleFormSubmit,
            userGroupList: userGroupList,
            userGroupSearchKeyword: userGroupSearchKeyword,
            loadMoreUserGroup: loadMoreUserGroup,
            handleSearchUserGroup: handleSearchUserGroup,
            addUserGroupModalVisible: addUserGroupModalVisible,
            handleOpenAddUserGroupModal: handleOpenAddUserGroupModal,
            handleCloseAddUserGroupModal: handleCloseAddUserGroupModal,
            addUserGroupForm: addUserGroupForm,
            addUserGroupFormRules: addUserGroupFormRules,
            addUserGroupFormRef: addUserGroupFormRef,
            addUserGroupFormSubmitLoading: addUserGroupFormSubmitLoading,
            handleAddUserGroupFormSubmit: handleAddUserGroupFormSubmit,
            handleOpenResetPwdModal: handleOpenResetPwdModal,
            handleCloseResetPwdModal: handleCloseResetPwdModal,
            resetPwdModalVisible: resetPwdModalVisible,
            resetPwdFormRef: resetPwdFormRef,
            resetPwdForm: resetPwdForm,
            resetPwdFormRules: resetPwdFormRules,
            handleGeneratePassword: handleGeneratePassword,
            handleResetPwdFormSubmit: handleResetPwdFormSubmit,
            resetPwdFormSubmitLoading: resetPwdFormSubmitLoading,
            handleCancelAuthorization: handleCancelAuthorization,
            handleToRoleDetail: handleToRoleDetail,
            hantoToUserGroupDetail: hantoToUserGroupDetail,
            authorizeVisible: authorizeVisible,
            handleAuthorize: handleAuthorize,
            handleToResourceGroupDetail: handleToResourceGroupDetail,
            handleToResourceDetail: handleToResourceDetail,
            handleToPermissionDetail: handleToPermissionDetail,
            accountLocked: accountLocked,
            enableMfa: enableMfa,
            handleSetAccountStatus: handleSetAccountStatus,
            handleSetMfaStatus: handleSetMfaStatus,
            handleRebindMfaDevice: handleRebindMfaDevice,
            handleClearAuthorizedTokens: handleClearAuthorizedTokens,
            consoleAccess: consoleAccess,
            handleSetConsoleAccessStatus: handleSetConsoleAccessStatus,
            permissionsPagination: permissionsPagination,
            authorizeSearchKeywords: authorizeSearchKeywords,
            handleGetUserPermissions: handleGetUserPermissions,
            resourceGroupNameFilter: resourceGroupNameFilter,
            resourceNameFilter: resourceNameFilter,
            permissionNameFilter: permissionNameFilter,
            permissionCodeFilter: permissionCodeFilter,
            handleResetPermissionFilter: handleResetPermissionFilter,
            allDictDatas: allDictDatas,
            loginLogs: loginLogs,
            loginLogsPagination: loginLogsPagination,
            handleLoginLogsPageChange: handleLoginLogsPageChange,
            handleLoginLogsPageSizeChange: handleLoginLogsPageSizeChange,
            handleClearAuthorizedTokensByLoginId: handleClearAuthorizedTokensByLoginId,
            passwordCheckerRef: passwordCheckerRef,
            checkPasswordLoading: checkPasswordLoading,
            checkPasswordRes: checkPasswordRes,
            handleCheckPassword: handleCheckPassword,
        };
    },
});
