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
import { computed, defineComponent, h, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { addRoleMapping, getRoleDetail, getRolePermissions, getRolePrincipals, removeRoleMapping, updateRole, } from "@/api/role";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { searchUser } from "@/api/user";
import { getUserGroupList } from "@/api/userGroup";
import { cancelAuthorization } from "@/api/permission";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import IconSearch from "@arco-design/web-vue/es/icon/icon-search";
import { usePagination } from "@/hooks/usePagination";
import { IconFilter } from "@arco-design/web-vue/es/icon";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("role_info");
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
    if (id === void 0) { id = roleId.value; }
    switch (tabKey) {
        case "role_info":
            handleGetRoleDetail(id);
            handleGetRolePrincipals(id);
            break;
        case "permission_management":
            handleGetRoleDetail(id);
            handleGetRolePermissions(id);
            break;
    }
};
var roleId = ref("");
var roleName = ref("");
/** 角色基本信息表单 */
var roleInfoFormRef = ref();
var roleInfoForm = reactive({
    id: undefined,
    name: undefined,
    code: undefined,
    desc: undefined,
});
var roleInfoFormRules = {
    name: [{ required: true, message: "角色名称未填写" }],
    code: [
        { required: true, message: "角色标识未填写" },
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
/**
 * 获取角色详情
 *
 * @param id 角色ID
 */
var handleGetRoleDetail = function (id) {
    getRoleDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            roleId.value = data.id;
            roleName.value = data.name;
            roleInfoForm.id = data.id;
            roleInfoForm.name = data.name;
            roleInfoForm.code = data.code;
            roleInfoForm.desc = data.desc;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取角色详情");
    });
};
/** 角色权限 */
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
// 权限标识过滤
var permissionCodeFilter = {
    slotName: "permission-code-filter",
    icon: function () {
        return authorizeFilteredFlags.permissionCode ? h(IconFilter) : h(IconSearch);
    },
};
/**
 * 获取角色权限
 */
var handleGetRolePermissions = function (id, page, size) {
    if (id === void 0) { id = roleId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getRolePermissions(id, {
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
        handleApiError(err, "获取角色权限");
    });
};
/**
 * 重置过滤
 */
var handleResetPermissionFilter = function (keyword) {
    authorizeSearchKeywords[keyword] = undefined;
    authorizeFilteredFlags[keyword] = false;
    handleGetRolePermissions();
};
/**
 * 提交角色基本信息表单
 */
var handleRoleInfoFormSubmit = function (formData) {
    updateRole(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetRoleDetail(roleId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新角色信息");
    });
};
/**
 * 重置角色基本信息表单
 */
var handleResetRoleInfoForm = function () {
    roleInfoFormRef.value.resetFields();
    handleGetRoleDetail(roleId.value);
};
/** 角色主体 */
var rolePrincipals = reactive([]);
var rolePrincipalsPagination;
/** 检索角色主体关键字 */
var searchRolePrincipalKeyword = ref("");
/**
 * 获取角色主体
 *
 * @param id 角色ID
 * @param page 页数
 * @param size 条数
 * @param keyword 用户名 / 用户组名检索关键字
 */
var handleGetRolePrincipals = function (id, page, size) {
    if (id === void 0) { id = roleId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getRolePrincipals(id, {
        page: page,
        size: size,
        keyword: searchRolePrincipalKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            rolePrincipals.length = 0;
            rolePrincipals.push.apply(rolePrincipals, data.list);
            rolePrincipalsPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取角色主体");
    });
};
/**
 * 检索角色主体
 */
var handleSearchRolePrincipal = function () {
    handleGetRolePrincipals();
};
/** 添加角色主体对话框 */
var addRolePrincipalModelVisible = ref(false);
/**
 * 打开添加角色主体对话框
 */
var handleOpenAddRolePrincipalModel = function () {
    handleGetAllUsers();
    handlGetAllUserGroups();
    addRolePrincipalModelVisible.value = true;
};
/**
 * 关闭添加角色主体对话框
 */
var handleCloseAddRolePrincipalModel = function () {
    addRolePrincipalModelVisible.value = false;
    allUsersPagination.current = 1;
    searchSelectUserGroupKeyword.value = "";
    searchSelectUserKeyword.value = "";
    activePrincipalTabKey.value = "1";
    addRolePrincipalsForm.userGroupIds.length = 0;
    addRolePrincipalsForm.userIds.length = 0;
    selectUserCheckAll.value = false;
    selectUserGroupCheckAll.value = false;
};
/** 主体 tab key */
var activePrincipalTabKey = ref("1");
/** 所有用户集合 */
var allUsers = reactive([]);
/** 搜索用户关键字 */
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
 * 搜索用户
 */
var handleSearchUser = function () {
    handleGetAllUsers(1);
};
/**
 * 滚动加载更多用户
 */
var allUsersContainerRef = ref();
var loadMoreUsersLoading = false;
var handleAllUsersContainerScroll = function () {
    if (loadMoreUsersLoading)
        return;
    var container = allUsersContainerRef.value;
    // 滚动到底部
    if (container.scrollTop + container.clientHeight >= container.scrollHeight) {
        // 有更多数据
        if (allUsers.length < allUsersPagination.total) {
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
/** 所有用户组集合 */
var allUserGroups = reactive([]);
var searchSelectUserGroupKeyword = ref("");
var allUserGroupsPagination = {
    current: 1,
    total: 0,
};
/**
 * 获取所有用户组
 */
var handlGetAllUserGroups = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserGroupList({
        page: page,
        size: size,
        keyword: searchSelectUserGroupKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                allUserGroups.length = 0;
                allUserGroups.push.apply(allUserGroups, data.list);
            }
            else {
                allUserGroups.push.apply(allUserGroups, data.list);
            }
            allUserGroupsPagination.current = data.current;
            allUserGroupsPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户组列表");
    });
};
/**
 * 搜索用户组
 */
var handleSearchUserGroup = function () {
    handlGetAllUserGroups(1);
};
/**
 * 滚动加载更多用户组
 */
var allUserGroupsContainerRef = ref();
var loadMoreUserGroupsLoading = false;
var handleAllUserGroupsContainerScroll = function () {
    if (loadMoreUserGroupsLoading)
        return;
    var container = allUserGroupsContainerRef.value;
    // 滚动到底部
    if (container.scrollTop + container.clientHeight >= container.scrollHeight) {
        // 有更多数据
        if (allUserGroups.length < allUserGroupsPagination.total) {
            loadMoreUserGroupsLoading = true;
            allUserGroupsPagination.current++;
            handlGetAllUserGroups(allUserGroupsPagination.current);
            loadMoreUserGroupsLoading = false;
            // 全选转换为半选
            if (selectUserGroupCheckAll.value) {
                selectUserGroupCheckAll.value = false;
                selectUserGroupIndeterminate.value = true;
            }
        }
    }
};
/** 添加角色主体表单 */
var addRolePrincipalsForm = reactive({
    userIds: [],
    userGroupIds: [],
});
/** 选择用户 / 用户组全选（半选） */
var selectUserCheckAll = ref(false);
var selectUserGroupCheckAll = ref(false);
var selectUserIndeterminate = ref(false);
var selectUserGroupIndeterminate = ref(false);
var handleChangeSelectUserCheckAll = function (value) {
    selectUserIndeterminate.value = false;
    // 全选
    if (value) {
        selectUserCheckAll.value = true;
        addRolePrincipalsForm.userIds.length = 0;
        allUsers.forEach(function (item) {
            addRolePrincipalsForm.userIds.push(item.userId);
        });
    }
    else {
        selectUserCheckAll.value = false;
        addRolePrincipalsForm.userIds = [];
    }
};
/**
 * 选择用户变化
 */
var handleSelectUserChange = function (selectedUsers) {
    // 半选状态变化
    if (selectedUsers.length == 0) {
        selectUserIndeterminate.value = false;
    }
    if (selectedUsers.length > 0 && selectedUsers.length < allUsers.length) {
        selectUserIndeterminate.value = true;
    }
};
var handleChangeSelectUserGroupCheckAll = function (value) {
    selectUserGroupIndeterminate.value = false;
    // 全选
    if (value) {
        selectUserGroupCheckAll.value = true;
        addRolePrincipalsForm.userGroupIds.length = 0;
        allUserGroups.forEach(function (item) {
            addRolePrincipalsForm.userGroupIds.push(item.id);
        });
    }
    else {
        selectUserGroupCheckAll.value = false;
        addRolePrincipalsForm.userGroupIds = [];
    }
};
/**
 * 选择用户组变化
 */
var handleSelectUserGroupChange = function (selectedUserGroups) {
    // 半选状态变化
    if (selectedUserGroups.length == 0) {
        selectUserGroupIndeterminate.value = false;
    }
    if (selectedUserGroups.length > 0 &&
        selectedUserGroups.length < allUserGroups.length) {
        selectUserGroupIndeterminate.value = true;
    }
};
/** 已选择的角色主体 */
var selectedPrincipals = computed(function () {
    var principals = [];
    principals.push.apply(principals, allUsers.filter(function (item) {
        return addRolePrincipalsForm.userIds.includes(item.userId);
    }));
    principals.push.apply(principals, allUserGroups.filter(function (item) {
        return addRolePrincipalsForm.userGroupIds.includes(item.id);
    }));
    return principals;
});
/**
 * 移除已选择的主体
 */
var handleRemoveSelectedPrincipal = function (principal) {
    if (principal.userId) {
        var targetIndex = addRolePrincipalsForm.userIds.findIndex(function (item) { return item === principal.userId; });
        addRolePrincipalsForm.userIds.splice(targetIndex, 1);
    }
    if (principal.id) {
        var targetIndex = addRolePrincipalsForm.userGroupIds.findIndex(function (item) { return item === principal.id; });
        addRolePrincipalsForm.userGroupIds.splice(targetIndex, 1);
    }
    // 全选和半选状态变化
    if (addRolePrincipalsForm.userIds.length == 0) {
        selectUserIndeterminate.value = false;
        selectUserCheckAll.value = false;
    }
    if (addRolePrincipalsForm.userIds.length > 0 &&
        addRolePrincipalsForm.userIds.length < allUsers.length) {
        selectUserIndeterminate.value = true;
    }
    if (addRolePrincipalsForm.userGroupIds.length == 0) {
        selectUserGroupIndeterminate.value = false;
        selectUserGroupCheckAll.value = false;
    }
    if (addRolePrincipalsForm.userGroupIds.length > 0 &&
        addRolePrincipalsForm.userGroupIds.length < allUserGroups.length) {
        selectUserGroupIndeterminate.value = true;
    }
};
/**
 * 清空已选择的主体
 */
var handleClearSelctedPrincipals = function () {
    selectUserIndeterminate.value = false;
    selectUserCheckAll.value = false;
    selectUserGroupIndeterminate.value = false;
    selectUserGroupCheckAll.value = false;
    addRolePrincipalsForm.userIds = [];
    addRolePrincipalsForm.userGroupIds = [];
};
/**
 * 提交添加角色主体表单
 */
var addRolePrincipalsFormSubmitLoading = ref(false);
var handleAddRolePrincipalsFormSubmit = function () {
    addRolePrincipalsFormSubmitLoading.value = true;
    addRoleMapping(__assign({ roleIds: [roleId.value] }, addRolePrincipalsForm))
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("添加成功");
            handleCloseAddRolePrincipalModel();
            handleGetRolePrincipals(roleId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "添加角色主体");
    })
        .finally(function () {
        addRolePrincipalsFormSubmitLoading.value = false;
    });
};
/**
 * 移除角色主体
 */
var handleRemoveRolePrincipal = function (principal) {
    Modal.warning({
        title: "\u786E\u5B9A\u79FB\u9664\u4E3B\u4F53\u300C".concat(principal.principal, "\u300D\u5417\uFF1F"),
        content: "此操作不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeRoleMapping({
                roleIds: [roleId.value],
                userIds: [principal.principalId],
            })
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("移除成功");
                    handleGetRolePrincipals(roleId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "移除角色主体");
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
            cancelAuthorization(permission.permissionId, roleId.value)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("取消授权成功");
                    handleGetRolePermissions(roleId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "取消授权");
            });
        },
    });
};
/**
 * 跳转主体详情
 *
 * @param principal 主体
 */
var handleToPrincipalDetail = function (principal) {
    if (principal.principalType === "USER") {
        handleToUserDetail(principal.principalId);
    }
    if (principal.principalType === "USER_GROUP") {
        hantoToUserGroupDetail(principal.principalId);
    }
};
/**
 * 跳转用户组详情
 *
 * @param userGroup 用户组
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
 *
 * @param user 用户信息
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
/** 授权对话框 */
var authorizeVisible = ref(false);
/**
 * 授权
 */
var handleAuthorize = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.authorizeOptions.principal = roleName.value;
    globalVariables.authorizeOptions.principalId = roleId.value;
    globalVariables.authorizeOptions.principalType = "ROLE";
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
export default defineComponent({
    setup: function () {
        var id = getQueryString("id");
        rolePrincipalsPagination = usePagination("".concat(id, "_rolePrincipals"), function (_a) {
            var page = _a.page, size = _a.size;
            if (getQueryString("active_tab") === "role_info") {
                handleGetRolePrincipals(id, page, size);
            }
        });
        permissionsPagination = usePagination("".concat(id, "_rolePermissions"), function (_a) {
            var page = _a.page, size = _a.size;
            if (getQueryString("active_tab") === "permission_management") {
                handleGetRolePermissions(id, page, size);
            }
        });
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "role_info";
            handleTabInit(activeTab.value, id);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            roleId: roleId,
            roleName: roleName,
            roleInfoFormRef: roleInfoFormRef,
            roleInfoForm: roleInfoForm,
            roleInfoFormRules: roleInfoFormRules,
            handleRoleInfoFormSubmit: handleRoleInfoFormSubmit,
            handleResetRoleInfoForm: handleResetRoleInfoForm,
            rolePrincipals: rolePrincipals,
            rolePrincipalsPagination: rolePrincipalsPagination,
            searchRolePrincipalKeyword: searchRolePrincipalKeyword,
            handleSearchRolePrincipal: handleSearchRolePrincipal,
            addRolePrincipalModelVisible: addRolePrincipalModelVisible,
            handleOpenAddRolePrincipalModel: handleOpenAddRolePrincipalModel,
            handleCloseAddRolePrincipalModel: handleCloseAddRolePrincipalModel,
            allUsers: allUsers,
            allUsersContainerRef: allUsersContainerRef,
            handleAllUsersContainerScroll: handleAllUsersContainerScroll,
            searchSelectUserKeyword: searchSelectUserKeyword,
            handleSearchUser: handleSearchUser,
            allUserGroups: allUserGroups,
            allUserGroupsContainerRef: allUserGroupsContainerRef,
            handleAllUserGroupsContainerScroll: handleAllUserGroupsContainerScroll,
            searchSelectUserGroupKeyword: searchSelectUserGroupKeyword,
            handleSearchUserGroup: handleSearchUserGroup,
            addRolePrincipalsForm: addRolePrincipalsForm,
            selectUserCheckAll: selectUserCheckAll,
            selectUserGroupCheckAll: selectUserGroupCheckAll,
            selectUserIndeterminate: selectUserIndeterminate,
            selectUserGroupIndeterminate: selectUserGroupIndeterminate,
            handleChangeSelectUserCheckAll: handleChangeSelectUserCheckAll,
            handleChangeSelectUserGroupCheckAll: handleChangeSelectUserGroupCheckAll,
            handleSelectUserChange: handleSelectUserChange,
            handleSelectUserGroupChange: handleSelectUserGroupChange,
            selectedPrincipals: selectedPrincipals,
            handleRemoveSelectedPrincipal: handleRemoveSelectedPrincipal,
            handleClearSelctedPrincipals: handleClearSelctedPrincipals,
            activePrincipalTabKey: activePrincipalTabKey,
            addRolePrincipalsFormSubmitLoading: addRolePrincipalsFormSubmitLoading,
            handleAddRolePrincipalsFormSubmit: handleAddRolePrincipalsFormSubmit,
            handleRemoveRolePrincipal: handleRemoveRolePrincipal,
            permissions: permissions,
            handleCancelAuthorization: handleCancelAuthorization,
            handleToPrincipalDetail: handleToPrincipalDetail,
            authorizeVisible: authorizeVisible,
            handleAuthorize: handleAuthorize,
            handleToResourceGroupDetail: handleToResourceGroupDetail,
            handleToResourceDetail: handleToResourceDetail,
            handleToPermissionDetail: handleToPermissionDetail,
            permissionsPagination: permissionsPagination,
            authorizeSearchKeywords: authorizeSearchKeywords,
            handleGetRolePermissions: handleGetRolePermissions,
            resourceGroupNameFilter: resourceGroupNameFilter,
            resourceNameFilter: resourceNameFilter,
            permissionNameFilter: permissionNameFilter,
            permissionCodeFilter: permissionCodeFilter,
            handleResetPermissionFilter: handleResetPermissionFilter,
        };
    },
});
