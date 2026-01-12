import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { getGroupResources } from "@/api/resourceGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { getResourcePermissions } from "@/api/resource";
import { authorize, getPermissionExpList } from "@/api/permission";
import { Notification } from "@arco-design/web-vue";
import { searchUser } from "@/api/user";
import { getUserGroupList } from "@/api/userGroup";
import { getRoleList } from "@/api/role";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
/** 资源组 */
var resourceGroup = ref({});
/** 是否选择授权主体 */
var princialSelectable = ref(false);
/** 授权表单 */
var authorizeFormRef = ref();
var authorizeForm = reactive({
    userIds: [],
    roleIds: [],
    userGroupIds: [],
    permissionIds: [],
    expressionIds: [],
    resourceId: undefined,
    priority: undefined,
});
var authorizeFormRules = {
    resourceId: [{ required: true, message: "请选择资源" }],
    permissionIds: [{ required: true, message: "请选择权限" }],
};
/** 授权主体表单 */
var principalFormRef = ref();
var principalForm = reactive({
    type: "USER",
    id: [],
});
var principalFormRules = {
    type: [{ required: true, message: "请选择主体类型" }],
    id: [{ required: true, message: "请选择授权主体" }],
};
/** 资源列表 */
var resourceList = reactive([]);
var resourceSearchKeyword = ref("");
var resourceListPagination = {
    current: 1,
    total: 0,
};
/**
 * 获取资源列表
 */
var handleGetResourceList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getGroupResources(resourceGroup.value.id, {
        page: page,
        size: size,
        keyword: resourceSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                resourceList.length = 0;
                resourceList.push.apply(resourceList, data.list);
            }
            else {
                resourceList.push.apply(resourceList, data.list);
            }
            resourceListPagination.current = data.current;
            resourceListPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取资源列表");
    });
};
/**
 * 搜索资源
 */
var handleSearchResource = function () {
    handleGetResourceList(1);
};
var loadMoreResourceLoading = false;
/**
 * 加载更多资源
 */
var loadMoreResource = function () {
    if (loadMoreResourceLoading)
        return;
    if (resourceList.length < resourceListPagination.total) {
        loadMoreResourceLoading = true;
        resourceListPagination.current++;
        handleGetResourceList(resourceListPagination.current);
        loadMoreResourceLoading = false;
    }
};
/** 权限列表 */
var permissionList = reactive([]);
var permissionSearchKeyword = ref("");
var permissionListPagination = {
    current: 1,
    total: 0,
};
/**
 * 获取权限列表
 */
var handleGetPermissionList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    if (!authorizeForm.resourceId)
        return;
    getResourcePermissions(authorizeForm.resourceId, {
        page: page,
        size: size,
        keyword: permissionSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                permissionList.length = 0;
                permissionList.push.apply(permissionList, data.list);
            }
            else {
                permissionList.push.apply(permissionList, data.list);
            }
            permissionListPagination.current = data.current;
            permissionListPagination.total = data.total;
            // 清空已选择的权限
            authorizeForm.permissionIds = [];
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取权限列表");
    });
};
/**
 * 搜索权限
 */
var handleSearchPermission = function () {
    handleGetPermissionList(1);
};
var loadMorePermissionLoading = false;
/**
 * 加载更多权限
 */
var loadMorePermission = function () {
    if (loadMorePermissionLoading)
        return;
    if (permissionList.length < permissionListPagination.total) {
        loadMorePermissionLoading = true;
        permissionListPagination.current++;
        handleGetPermissionList(permissionListPagination.current);
        loadMorePermissionLoading = false;
    }
};
/**
 * 选择权限下拉框显示变化
 */
var handlePermissionSelectVisibleChange = function (visible) {
    if (visible) {
        handleGetAuthorizeConditionList(1);
    }
};
/** 限制条件列表 */
var authorizeConditionList = reactive([]);
var authorizeConditionSearchKeyword = ref("");
var authorizeConditionListPagination = {
    total: 0,
    current: 1,
};
/**
 * 获取限制条件
 */
var handleGetAuthorizeConditionList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getPermissionExpList({
        page: page,
        size: size,
        keyword: authorizeConditionSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                authorizeConditionList.length = 0;
                authorizeConditionList.push.apply(authorizeConditionList, data.list);
            }
            else {
                authorizeConditionList.push.apply(authorizeConditionList, data.list);
            }
            authorizeConditionListPagination.current = data.current;
            authorizeConditionListPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取权限表达式列表");
    });
};
/**
 * 搜索限制条件
 */
var handleSearchAuthorizeCondition = function () {
    handleGetAuthorizeConditionList(1);
};
/**
 * 加载更多限制条件
 */
var loadMoreAuthorizeConditionLoading = false;
var loadMoreAuthorizeCondition = function () {
    if (loadMoreAuthorizeConditionLoading)
        return;
    if (authorizeConditionList.length < authorizeConditionListPagination.total) {
        loadMoreAuthorizeConditionLoading = true;
        authorizeConditionListPagination.current++;
        handleGetAuthorizeConditionList(authorizeConditionListPagination.current);
        loadMoreAuthorizeConditionLoading = false;
    }
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
/** 角色列表 */
var roleList = reactive([]);
var roleSearchKeyword = ref("");
var roleListPagination = {
    current: 1,
    total: 0,
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
 * 选择主体类型变化
 */
var principalSelectChange = function (value) {
    principalForm.id.length = 0;
    if (value === "USER") {
        handleGetUserList(1);
    }
    if (value === "USER_GROUP") {
        handleGetUserGroupList(1);
    }
    if (value === "ROLE") {
        handleGetRoleList(1);
    }
};
/**
 * 提交授权表单
 */
var handleAuthorizeFormSubmit = function () {
    if (princialSelectable.value) {
        principalFormRef.value.validate(function (errors) {
            var _a, _b, _c;
            if (!errors) {
                if (principalForm.type === "USER") {
                    (_a = authorizeForm.userIds).push.apply(_a, principalForm.id);
                }
                if (principalForm.type === "USER_GROUP") {
                    (_b = authorizeForm.userGroupIds).push.apply(_b, principalForm.id);
                }
                if (principalForm.type === "ROLE") {
                    (_c = authorizeForm.roleIds).push.apply(_c, principalForm.id);
                }
            }
        });
    }
    authorizeFormRef.value.validate(function (errors) {
        if (!errors) {
            authorize(authorizeForm)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("创建成功");
                    handleResetAuthorizeForm();
                });
            })
                .catch(function (err) {
                handleApiError(err, "创建授权");
            });
        }
    });
};
/**
 * 重置授权表单
 */
var handleResetAuthorizeForm = function () {
    if (princialSelectable.value) {
        principalFormRef.value.resetFields();
    }
    authorizeFormRef.value.clearValidate();
    authorizeForm.resourceId = undefined;
    authorizeForm.priority = undefined;
    authorizeForm.expressionIds = [];
    authorizeForm.permissionIds = [];
    handleGetResourceList();
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            var globalVariablesStore = useGlobalVariablesStore();
            var authorizeOptions = globalVariablesStore.getData().authorizeOptions;
            if (authorizeOptions.resourceGroup) {
                resourceGroup.value = authorizeOptions.resourceGroup;
                handleGetResourceList();
            }
            if (authorizeOptions.principalType) {
                if (authorizeOptions.principalType === "USER") {
                    authorizeForm.userIds = [authorizeOptions.principalId];
                }
                if (authorizeOptions.principalType === "USER_GROUP") {
                    authorizeForm.userGroupIds = [authorizeOptions.principalId];
                }
                if (authorizeOptions.principalType === "ROLE") {
                    authorizeForm.roleIds = [authorizeOptions.principalId];
                }
            }
            if (!authorizeOptions.principal) {
                princialSelectable.value = true;
                handleGetUserList(1);
            }
            else {
                princialSelectable.value = false;
            }
        });
        return {
            handleBack: handleBack,
            princialSelectable: princialSelectable,
            resourceGroup: resourceGroup,
            authorizeForm: authorizeForm,
            authorizeFormRef: authorizeFormRef,
            resourceList: resourceList,
            resourceListPagination: resourceListPagination,
            resourceSearchKeyword: resourceSearchKeyword,
            handleSearchResource: handleSearchResource,
            loadMoreResource: loadMoreResource,
            authorizeFormRules: authorizeFormRules,
            permissionList: permissionList,
            permissionSearchKeyword: permissionSearchKeyword,
            permissionListPagination: permissionListPagination,
            handleSearchPermission: handleSearchPermission,
            loadMorePermission: loadMorePermission,
            handlePermissionSelectVisibleChange: handlePermissionSelectVisibleChange,
            authorizeConditionList: authorizeConditionList,
            authorizeConditionSearchKeyword: authorizeConditionSearchKeyword,
            handleSearchAuthorizeCondition: handleSearchAuthorizeCondition,
            loadMoreAuthorizeCondition: loadMoreAuthorizeCondition,
            handleAuthorizeFormSubmit: handleAuthorizeFormSubmit,
            handleResetAuthorizeForm: handleResetAuthorizeForm,
            principalFormRef: principalFormRef,
            principalForm: principalForm,
            principalFormRules: principalFormRules,
            userList: userList,
            userSearchKeyword: userSearchKeyword,
            loadMoreUser: loadMoreUser,
            handleSearchUser: handleSearchUser,
            userGroupList: userGroupList,
            userGroupSearchKeyword: userGroupSearchKeyword,
            loadMoreUserGroup: loadMoreUserGroup,
            handleSearchUserGroup: handleSearchUserGroup,
            roleList: roleList,
            roleSearchKeyword: roleSearchKeyword,
            loadMoreRole: loadMoreRole,
            handleSearchRole: handleSearchRole,
            principalSelectChange: principalSelectChange,
        };
    },
});
