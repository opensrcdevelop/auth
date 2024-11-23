import { defineComponent, onMounted, onUpdated, reactive, ref } from "vue";
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
const handleBack = () => {
  router.back();
};

/** 资源组 */
const resourceGroup = ref({} as any);

/** 是否选择授权主体 */
const princialSelectable = ref(false);

/** 授权表单 */
const authorizeFormRef = ref();
const authorizeForm = reactive({
  userIds: [],
  roleIds: [],
  userGroupIds: [],
  permissionIds: [],
  expressionIds: [],
  resourceId: undefined,
});
const authorizeFormRules = {
  resourceId: [{ required: true, message: "请选择资源" }],
  permissionIds: [{ required: true, message: "请选择权限" }],
};

/** 授权主体表单 */
const principalFormRef = ref();
const principalForm = reactive({
  type: "USER",
  id: [],
});
const principalFormRules = {
  type: [{ required: true, message: "请选择主体类型" }],
  id: [{ required: true, message: "请选择授权主体" }],
};

/** 资源列表 */
const resourceList = reactive([]);
const resourceSearchKeyword = ref("");
const resourceListPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取资源列表
 */
const handleGetResourceList = (page: number = 1, size: number = 15) => {
  getGroupResources(resourceGroup.value.id, {
    page,
    size,
    keyword: resourceSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page == 1) {
          resourceList.length = 0;
          resourceList.push(...data.list);
        } else {
          resourceList.push(...data.list);
        }
        resourceListPagination.current = data.current;
        resourceListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取资源列表");
    });
};

/**
 * 搜索资源
 */
const handleSearchResource = () => {
  handleGetResourceList(1);
};

let loadMoreResourceLoading = false;
/**
 * 加载更多资源
 */
const loadMoreResource = () => {
  if (loadMoreResourceLoading) return;
  if (resourceList.length < resourceListPagination.total) {
    loadMoreResourceLoading = true;
    resourceListPagination.current++;
    handleGetResourceList(resourceListPagination.current);
    loadMoreResourceLoading = false;
  }
};

/** 权限列表 */
const permissionList = reactive([]);
const permissionSearchKeyword = ref("");
const permissionListPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取权限列表
 */
const handleGetPermissionList = (page: number = 1, size: number = 15) => {
  if (!authorizeForm.resourceId) return;
  getResourcePermissions(authorizeForm.resourceId, {
    page,
    size,
    keyword: permissionSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page == 1) {
          permissionList.length = 0;
          permissionList.push(...data.list);
        } else {
          permissionList.push(...data.list);
        }
        permissionListPagination.current = data.current;
        permissionListPagination.total = data.total;

        // 清空已选择的权限
        authorizeForm.permissionIds = [];
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限列表");
    });
};

/**
 * 搜索权限
 */
const handleSearchPermission = () => {
  handleGetPermissionList(1);
};

let loadMorePermissionLoading = false;
/**
 * 加载更多权限
 */
const loadMorePermission = () => {
  if (loadMorePermissionLoading) return;
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
const handlePermissionSelectVisibleChange = (visible: boolean) => {
  if (visible) {
    handleGetAuthorizeConditionList(1);
  }
};

/** 限制条件列表 */
const authorizeConditionList = reactive([]);
const authorizeConditionSearchKeyword = ref("");
const authorizeConditionListPagination = {
  total: 0,
  current: 1,
};

/**
 * 获取限制条件
 */
const handleGetAuthorizeConditionList = (
  page: number = 1,
  size: number = 15
) => {
  getPermissionExpList({
    page,
    size,
    keyword: authorizeConditionSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page == 1) {
          authorizeConditionList.length = 0;
          authorizeConditionList.push(...data.list);
        } else {
          authorizeConditionList.push(...data.list);
        }
        authorizeConditionListPagination.current = data.current;
        authorizeConditionListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限表达式列表");
    });
};

/**
 * 搜索限制条件
 */
const handleSearchAuthorizeCondition = () => {
  handleGetAuthorizeConditionList(1);
};

/**
 * 加载更多限制条件
 */
let loadMoreAuthorizeConditionLoading = false;
const loadMoreAuthorizeCondition = () => {
  if (loadMoreAuthorizeConditionLoading) return;
  if (authorizeConditionList.length < authorizeConditionListPagination.total) {
    loadMoreAuthorizeConditionLoading = true;
    authorizeConditionListPagination.current++;
    handleGetAuthorizeConditionList(authorizeConditionListPagination.current);
    loadMoreAuthorizeConditionLoading = false;
  }
};

/** 用户列表 */
const userList = reactive([]);
const userSearchKeyword = ref("");
const userListPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取用户列表
 */
const handleGetUserList = (page: number = 1, size: number = 15) => {
  searchUser(userSearchKeyword.value, {
    page,
    size,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      if (page == 1) {
        userList.length = 0;
        userList.push(...data.list);
      } else {
        userList.push(...data.list);
      }
      userListPagination.current = data.current;
      userListPagination.total = data.total;
    });
  });
};

/**
 * 搜索用户
 */
const handleSearchUser = () => {
  handleGetUserList(1);
};

/**
 * 加载更多用户
 */
let loadMoreUserLoading = false;
const loadMoreUser = () => {
  if (loadMoreUserLoading) return;
  if (userList.length < userListPagination.total) {
    loadMoreUserLoading = true;
    userListPagination.current++;
    handleGetUserList(userListPagination.current);
    loadMoreUserLoading = false;
  }
};

/** 用户组列表 */
const userGroupList = reactive([]);
const userGroupSearchKeyword = ref("");
const userGroupListPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取用户组列表
 */
const handleGetUserGroupList = (page: number = 1, size: number = 15) => {
  getUserGroupList({
    page,
    size,
    keyword: userGroupSearchKeyword.value,
  }).then((result: any) => {
    handleApiSuccess(result, (data: any) => {
      if (page == 1) {
        userGroupList.length = 0;
        userGroupList.push(...data.list);
      } else {
        userGroupList.push(...data.list);
      }
      userGroupListPagination.current = data.current;
      userGroupListPagination.total = data.total;
    });
  });
};

/**
 * 搜索用户组
 */
const handleSearchUserGroup = () => {
  handleGetUserGroupList(1);
};

/**
 * 加载更多用户组
 */
let loadMoreUserGroupLoading = false;
const loadMoreUserGroup = () => {
  if (loadMoreUserGroupLoading) return;
  if (userGroupList.length < userGroupListPagination.total) {
    loadMoreUserGroupLoading = true;
    userGroupListPagination.current++;
    handleGetUserGroupList(userGroupListPagination.current);
    loadMoreUserGroupLoading = false;
  }
};

/** 角色列表 */
const roleList = reactive([]);
const roleSearchKeyword = ref("");
const roleListPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取角色列表
 */
const handleGetRoleList = (page: number = 1, size: number = 15) => {
  getRoleList({
    page,
    size,
    keyword: roleSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page == 1) {
          roleList.length = 0;
          roleList.push(...data.list);
        } else {
          roleList.push(...data.list);
        }
        roleListPagination.current = data.current;
        roleListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取角色列表");
    });
};

/**
 * 搜索角色
 */
const handleSearchRole = () => {
  handleGetRoleList(1);
};

/**
 * 加载更多角色
 */
let loadMoreRoleLoading = false;
const loadMoreRole = () => {
  if (loadMoreRoleLoading) return;
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
const principalSelectChange = (value: any) => {
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
const handleAuthorizeFormSubmit = () => {
  if (princialSelectable.value) {
    principalFormRef.value.validate((errors) => {
      if (!errors) {
        if (principalForm.type === "USER") {
          authorizeForm.userIds.push(...principalForm.id);
        }

        if (principalForm.type === "USER_GROUP") {
          authorizeForm.userGroupIds.push(...principalForm.id);
        }

        if (principalForm.type === "ROLE") {
          authorizeForm.roleIds.push(...principalForm.id);
        }
      }
    });
  }

  authorizeFormRef.value.validate((errors) => {
    if (!errors) {
      authorize(authorizeForm)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("创建成功");
            handleResetAuthorizeForm();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "创建授权");
        });
    }
  });
};

/**
 * 重置授权表单
 */
const handleResetAuthorizeForm = () => {
  if (princialSelectable.value) {
    principalFormRef.value.resetFields();
  }

  authorizeFormRef.value.clearValidate();
  authorizeForm.resourceId = undefined;
  authorizeForm.expressionIds = [];
  authorizeForm.permissionIds = [];
  handleGetResourceList();
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const globalVariablesStore = useGlobalVariablesStore();
      const authorizeOptions = globalVariablesStore.getData().authorizeOptions;
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
      } else {
        princialSelectable.value = false;
      }
    });

    return {
      handleBack,
      princialSelectable,
      resourceGroup,
      authorizeForm,
      authorizeFormRef,
      resourceList,
      resourceListPagination,
      resourceSearchKeyword,
      handleSearchResource,
      loadMoreResource,
      authorizeFormRules,
      permissionList,
      permissionSearchKeyword,
      permissionListPagination,
      handleSearchPermission,
      loadMorePermission,
      handlePermissionSelectVisibleChange,
      authorizeConditionList,
      authorizeConditionSearchKeyword,
      handleSearchAuthorizeCondition,
      loadMoreAuthorizeCondition,
      handleAuthorizeFormSubmit,
      handleResetAuthorizeForm,
      principalFormRef,
      principalForm,
      principalFormRules,
      userList,
      userSearchKeyword,
      loadMoreUser,
      handleSearchUser,
      userGroupList,
      userGroupSearchKeyword,
      loadMoreUserGroup,
      handleSearchUserGroup,
      roleList,
      roleSearchKeyword,
      loadMoreRole,
      handleSearchRole,
      principalSelectChange,
    };
  },
});
