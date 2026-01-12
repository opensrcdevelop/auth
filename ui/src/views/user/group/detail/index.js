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
import { addUserGroupMapping, getGroupUsers, getUserGroupDetail, getUserGroupPermissions, removeUserGroupMapping, updateUserGroup, } from "@/api/userGroup";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { searchUser } from "@/api/user";
import { cancelAuthorization } from "@/api/permission";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import IconSearch from "@arco-design/web-vue/es/icon/icon-search";
import { usePagination } from "@/hooks/usePagination";
import { IconFilter } from "@arco-design/web-vue/es/icon";
import UserGroupConditions from "../components/UserGroupConditions.vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("user_group_info");
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
    if (tabKey === "permission_management") {
        handleGetUserGroupPermissions();
    }
};
var userGroupName = ref("");
var userGroupId = ref("");
/** 用户组基本信息表单 */
var userGroupInfoFormRef = ref();
var userGroupInfoForm = reactive({
    id: undefined,
    name: undefined,
    code: undefined,
    desc: undefined,
    type: undefined,
    conditions: undefined,
});
var userGroupInfoFormRules = {
    name: [{ required: true, message: "用户组名称未填写" }],
    code: [
        { required: true, message: "用户组标识未填写" },
        {
            validator: function (value, cb) {
                if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
                    cb("只允许包含英文字母、数字、下划线_、横线-");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
var userGroupConditionsRef = ref();
/**
 * 获取用户组详情
 *
 * @param id 用户组id
 */
var handleGetUserGroupDetail = function (id) {
    getUserGroupDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userGroupName.value = data.name;
            userGroupId.value = data.id;
            userGroupInfoForm.id = data.id;
            userGroupInfoForm.name = data.name;
            userGroupInfoForm.code = data.code;
            userGroupInfoForm.desc = data.desc;
            userGroupInfoForm.type = data.type;
            userGroupInfoForm.conditions = data.conditions;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户组详情");
    });
};
/** 过滤标记 */
var authorizeFilteredFlags = reactive({
    resourceGroupName: false,
    resourceName: false,
    permissionName: false,
    permissionCode: false,
});
/** 用户组权限 */
var permissions = reactive([]);
var permissionsPagination;
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
// 资源组名称过滤
var resourceGroupNameFilter = {
    filter: function (value, record) {
        authorizeSearchKeywords.resourceGroupName = value;
        handleGetUserGroupPermissions();
    },
    slotName: "resource-group-name-filter",
    icon: function () { return h(IconSearch); },
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
// 权限标识过滤
var permissionCodeFilter = {
    slotName: "permission-code-filter",
    icon: function () { return h(IconSearch); },
};
/**
 * 获取用户组权限
 */
var handleGetUserGroupPermissions = function (id, page, size) {
    if (id === void 0) { id = userGroupId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserGroupPermissions(id, {
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
        handleApiError(err, "获取用户组权限");
    });
};
/**
 * 重置过滤
 */
var handleResetPermissionFilter = function (keyword) {
    authorizeSearchKeywords[keyword] = undefined;
    authorizeFilteredFlags[keyword] = false;
    handleGetUserGroupPermissions();
};
var groupUsers = reactive([]);
var groupUsersPagination;
/**
 * 获取组内用户
 *
 * @param id 用户组 ID
 * @param page 页数
 * @param size 条数
 * @param keyword 用户名 / 邮箱 / 手机号 搜索关键字
 */
var handleGetGroupUsers = function (id, page, size, keyword) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    if (keyword === void 0) { keyword = ""; }
    getGroupUsers(id, {
        page: page,
        size: size,
        keyword: keyword,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            groupUsers.length = 0;
            groupUsers.push.apply(groupUsers, data.list);
            groupUsersPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取组内用户");
    });
};
/**
 * 提交用户组信息表单
 *
 */
var handleUserGroupInfoFormSubmit = function () { return __awaiter(void 0, void 0, void 0, function () {
    var validateRes1, validateRes2;
    return __generator(this, function (_a) {
        switch (_a.label) {
            case 0: return [4 /*yield*/, userGroupInfoFormRef.value.validate()];
            case 1:
                validateRes1 = _a.sent();
                validateRes2 = true;
                if (!userGroupConditionsRef.value) return [3 /*break*/, 3];
                return [4 /*yield*/, userGroupConditionsRef.value.validate()];
            case 2:
                validateRes2 = _a.sent();
                _a.label = 3;
            case 3:
                if (validateRes1 !== undefined || !validateRes2) {
                    return [2 /*return*/];
                }
                updateUserGroup(userGroupInfoForm)
                    .then(function (result) {
                    handleApiSuccess(result, function () {
                        Notification.success("保存成功");
                        handleGetUserGroupDetail(userGroupId.value);
                        if (userGroupInfoForm.type === "DYNAMIC") {
                            handleGetGroupUsers(userGroupId.value);
                        }
                    });
                })
                    .catch(function (err) {
                    handleApiError(err, "更新用户组信息");
                });
                return [2 /*return*/];
        }
    });
}); };
/**
 * 重置用户组信息表单
 */
var handleResetUserGroupInfoForm = function () {
    handleGetUserGroupDetail(userGroupId.value);
    userGroupInfoFormRef.value.resetFields();
};
/** 用户组成员搜索关键字 */
var searchGroupUserKeyword = ref("");
/**
 * 搜索用户组成员
 */
var handleSearchGroupUser = function () {
    handleGetGroupUsers(userGroupId.value, 1, 15, searchGroupUserKeyword.value);
};
var addGroupUserModalVisible = ref(false);
/**
 * 打开添加用户组成员对话框
 */
var handleOpenAddGroupUserModal = function () {
    addGroupUserModalVisible.value = true;
    handleGetAllUsers();
};
/**
 * 关闭添加用户组成员对话框
 */
var handleCloseAddGroupUserModal = function () {
    addGroupUserModalVisible.value = false;
    addGroupUsersForm.userIds = [];
    searchSelectUserKeyword.value = "";
    allUsers.length = 0;
    allUsersPagination.current = 1;
    selectUserIndeterminate.value = false;
    selectUserCheckAll.value = false;
};
/** 所有用户集合 */
var allUsers = reactive([]);
/** 搜索成员关键字 */
var searchSelectUserKeyword = ref("");
var allUsersPagination = {
    current: 1,
    total: 0,
};
/**
 * 获取全部用户
 *
 * @param page 页数
 * @param size 分页大小
 */
var handleGetAllUsers = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    searchUser(searchSelectUserKeyword.value, {
        page: page,
        size: size,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                allUsers.length = 0;
                allUsers.push.apply(allUsers, data.list);
            }
            else {
                allUsers.push.apply(allUsers, data.list);
            }
            allUsersPagination.current = data.current;
            allUsersPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户列表");
    });
};
/**
 * 搜索成员
 */
var handleSearchUser = function () {
    handleGetAllUsers(1);
};
/**
 * 滚动加载更多用户
 */
var allUsersContainerRef = ref(null);
var loadMoreUsersLoading = false;
var handleAllUsersContainerScroll = function () {
    var container = allUsersContainerRef.value;
    // 滚动到底部
    if (container.scrollTop + container.clientHeight >= container.scrollHeight) {
        // 有更多数据
        if (allUsers.length < allUsersPagination.total) {
            if (loadMoreUsersLoading)
                return;
            loadMoreUsersLoading = true;
            allUsersPagination.current++;
            handleGetAllUsers(allUsersPagination.current);
            loadMoreUsersLoading = false;
            // 全选转换为半选
            if (selectUserCheckAll.value) {
                selectUserCheckAll.value = false;
                selectUserIndeterminate.value = true;
            }
        }
    }
};
/** 添加成员表单 */
var addGroupUsersForm = reactive({
    userIds: [],
});
/** 选择成员全选 */
var selectUserCheckAll = ref(false);
/** 选择成员半选 */
var selectUserIndeterminate = ref(false);
/**
 * 选择成员全选变化
 */
var handleChangeCheckAll = function (value) {
    selectUserIndeterminate.value = false;
    // 全选
    if (value) {
        selectUserCheckAll.value = true;
        addGroupUsersForm.userIds.length = 0;
        allUsers.forEach(function (item) {
            addGroupUsersForm.userIds.push(item.userId);
        });
    }
    else {
        selectUserCheckAll.value = false;
        addGroupUsersForm.userIds = [];
    }
};
/** 选择成员变化 */
var handleSelectUserChange = function (selectedUsers) {
    // 半选状态变化
    if (selectedUsers.length == 0) {
        selectUserIndeterminate.value = false;
    }
    if (selectedUsers.length > 0 && selectedUsers.length < allUsers.length) {
        selectUserIndeterminate.value = true;
    }
};
/**
 * 已选择的成员
 */
var slectedUsers = computed(function () {
    return allUsers.filter(function (item) {
        return addGroupUsersForm.userIds.includes(item.userId);
    });
});
/**
 * 移除已选择的成员
 */
var handleRemoveSelectedUser = function (userId) {
    var targetIndex = addGroupUsersForm.userIds.findIndex(function (item) { return item === userId; });
    addGroupUsersForm.userIds.splice(targetIndex, 1);
    // 全选和半选状态变化
    if (addGroupUsersForm.userIds.length == 0) {
        selectUserIndeterminate.value = false;
        selectUserCheckAll.value = false;
    }
    if (addGroupUsersForm.userIds.length > 0 &&
        addGroupUsersForm.userIds.length < allUsers.length) {
        selectUserIndeterminate.value = true;
    }
};
/**
 * 清空已选择的成员
 */
var handleClearSelctedUsers = function () {
    selectUserCheckAll.value = false;
    selectUserIndeterminate.value = false;
    addGroupUsersForm.userIds = [];
};
/**
 * 添加成员表单提交
 */
var addGroupUsersFormSubmitLoading = ref(false);
var handleAddGroupUsersFormSubmit = function () {
    if (addGroupUsersForm.userIds.length > 0) {
        addGroupUsersFormSubmitLoading.value = true;
        addUserGroupMapping({
            userGroupIds: [userGroupId.value],
            userIds: addGroupUsersForm.userIds,
        })
            .then(function (result) {
            handleApiSuccess(result, function () {
                Notification.success("添加成员成功");
                handleCloseAddGroupUserModal();
                handleGetGroupUsers(userGroupId.value);
            });
        })
            .catch(function (err) {
            handleApiError(err, "添加成员");
        })
            .finally(function () {
            addGroupUsersFormSubmitLoading.value = false;
        });
    }
};
/**
 * 移除用户组成员
 */
var handleRemoveGroupUser = function (user) {
    Modal.warning({
        title: "\u786E\u5B9A\u79FB\u9664\u6210\u5458\u300C".concat(user.username, "\u300D\u5417\uFF1F"),
        content: "此操作不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeUserGroupMapping({
                userGroupIds: [userGroupId.value],
                userIds: [user.id],
            })
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("移除成功");
                    handleGetGroupUsers(userGroupId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "移除成员");
            });
        },
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
            cancelAuthorization(permission.permissionId, userGroupId.value)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("取消授权成功");
                    handleGetUserGroupPermissions(userGroupId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "取消授权");
            });
        },
    });
};
/**
 * 跳转用户详情
 *
 * @param user 用户信息
 */
var handleToUserDetail = function (user) {
    router.push({
        path: "/user/detail",
        query: {
            id: user.id,
            active_tab: "user_info",
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
    globalVariables.authorizeOptions.principal = userGroupName.value;
    globalVariables.authorizeOptions.principalId = userGroupId.value;
    globalVariables.authorizeOptions.principalType = "USER_GROUP";
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
export default defineComponent({
    components: {
        UserGroupConditions: UserGroupConditions,
    },
    setup: function () {
        var userGroupId = getQueryString("id");
        groupUsersPagination = usePagination("".concat(userGroupId, "_groupUsers"), function (_a) {
            var page = _a.page, size = _a.size;
            handleGetGroupUsers(userGroupId, page, size);
        });
        permissionsPagination = usePagination("".concat(userGroupId, "_userGroupPermissions"), function (_a) {
            var page = _a.page, size = _a.size;
            if (getQueryString("active_tab") === "permission_management") {
                handleGetUserGroupPermissions(userGroupId, page, size);
            }
        });
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "user_group_info";
            handleGetUserGroupDetail(getQueryString("id"));
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            userGroupName: userGroupName,
            userGroupId: userGroupId,
            groupUsers: groupUsers,
            groupUsersPagination: groupUsersPagination,
            userGroupInfoForm: userGroupInfoForm,
            userGroupInfoFormRef: userGroupInfoFormRef,
            userGroupInfoFormRules: userGroupInfoFormRules,
            handleUserGroupInfoFormSubmit: handleUserGroupInfoFormSubmit,
            handleResetUserGroupInfoForm: handleResetUserGroupInfoForm,
            searchGroupUserKeyword: searchGroupUserKeyword,
            handleSearchGroupUser: handleSearchGroupUser,
            addGroupUserModalVisible: addGroupUserModalVisible,
            handleOpenAddGroupUserModal: handleOpenAddGroupUserModal,
            handleCloseAddGroupUserModal: handleCloseAddGroupUserModal,
            allUsers: allUsers,
            selectUserCheckAll: selectUserCheckAll,
            selectUserIndeterminate: selectUserIndeterminate,
            addGroupUsersForm: addGroupUsersForm,
            handleChangeCheckAll: handleChangeCheckAll,
            handleSelectUserChange: handleSelectUserChange,
            slectedUsers: slectedUsers,
            handleRemoveSelectedUser: handleRemoveSelectedUser,
            searchSelectUserKeyword: searchSelectUserKeyword,
            handleGetAllUsers: handleGetAllUsers,
            handleSearchUser: handleSearchUser,
            allUsersContainerRef: allUsersContainerRef,
            handleAllUsersContainerScroll: handleAllUsersContainerScroll,
            handleClearSelctedUsers: handleClearSelctedUsers,
            addGroupUsersFormSubmitLoading: addGroupUsersFormSubmitLoading,
            handleAddGroupUsersFormSubmit: handleAddGroupUsersFormSubmit,
            handleRemoveGroupUser: handleRemoveGroupUser,
            permissions: permissions,
            handleCancelAuthorization: handleCancelAuthorization,
            handleToUserDetail: handleToUserDetail,
            authorizeVisible: authorizeVisible,
            handleAuthorize: handleAuthorize,
            handleToResourceGroupDetail: handleToResourceGroupDetail,
            handleToResourceDetail: handleToResourceDetail,
            handleToPermissionDetail: handleToPermissionDetail,
            permissionsPagination: permissionsPagination,
            authorizeSearchKeywords: authorizeSearchKeywords,
            handleGetUserGroupPermissions: handleGetUserGroupPermissions,
            resourceGroupNameFilter: resourceGroupNameFilter,
            resourceNameFilter: resourceNameFilter,
            permissionNameFilter: permissionNameFilter,
            permissionCodeFilter: permissionCodeFilter,
            handleResetPermissionFilter: handleResetPermissionFilter,
            userGroupConditionsRef: userGroupConditionsRef,
        };
    },
});
