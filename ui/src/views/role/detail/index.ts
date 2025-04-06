import {computed, defineComponent, h, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {
  addRoleMapping,
  getRoleDetail,
  getRolePermissions,
  getRolePrincipals,
  removeRoleMapping,
  updateRole,
} from "@/api/role";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {searchUser} from "@/api/user";
import {getUserGroupList} from "@/api/userGroup";
import {cancelAuthorization} from "@/api/permission";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import IconSearch from "@arco-design/web-vue/es/icon/icon-search";
import {usePagination} from "@/hooks/usePagination";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("role_info");

/**
 * tab 切换事件
 *
 * @param tabKey tabKey
 */
const handleTabChange = (tabKey: string) => {
  router.replace({
    query: {
      ...router.currentRoute.value.query,
      active_tab: tabKey,
    },
  });
  activeTab.value = tabKey;
  if (activeTab.value === "permission_management") {
    handleGetRolePermissions();
  }
};

const roleId = ref("");
const roleName = ref("");

/** 角色基本信息表单 */
const roleInfoFormRef = ref();
const roleInfoForm = reactive({
  id: undefined,
  name: undefined,
  code: undefined,
  desc: undefined,
});
const roleInfoFormRules = {
  name: [{ required: true, message: "角色名称未填写" }],
  code: [
    { required: true, message: "角色标识未填写" },
    {
      validator: (value, cb) => {
        if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
          cb("只允许包含英文字母、数字、下划线_、横线-");
        } else {
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
const handleGetRoleDetail = (id: string) => {
  getRoleDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        roleId.value = data.id;
        roleName.value = data.name;

        roleInfoForm.id = data.id;
        roleInfoForm.name = data.name;
        roleInfoForm.code = data.code;
        roleInfoForm.desc = data.desc;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取角色详情");
    });
};

/** 角色权限 */
const permissions = reactive([]);
let permissionsPagination;
const authorizeSearchKeywords = reactive({
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
const resourceGroupNameFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.resourceGroupName = value;
    handleGetRolePermissions();
  },
  slotName: "resource-group-name-filter",
  icon: () => h(IconSearch),
};
// 资源名称过滤
const resourceNameFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.resourceName = value;
    handleGetRolePermissions();
  },
  slotName: "resource-name-filter",
  icon: () => h(IconSearch),
};
// 资源名称过滤
const permissionNameFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.permissionName = value;
    handleGetRolePermissions();
  },
  slotName: "permission-name-filter",
  icon: () => h(IconSearch),
};
// 资源标识过滤
const permissionCodeFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.permissionCode = value;
    handleGetRolePermissions();
  },
  slotName: "permission-code-filter",
  icon: () => h(IconSearch),
};

/**
 * 获取角色权限
 */
const handleGetRolePermissions = (
  id: string = roleId.value,
  page: number = 1,
  size: number = 15
) => {
  getRolePermissions(id, {
    page,
    size,
    resourceGroupNameSearchKeyword: authorizeSearchKeywords.resourceGroupName,
    resourceNameSearchKeyword: authorizeSearchKeywords.resourceName,
    permissionNameSearchKeyword: authorizeSearchKeywords.permissionName,
    permissionCodeSearchKeyword: authorizeSearchKeywords.permissionCode,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissions.length = 0;
        permissions.push(...data.list);

        permissionsPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取角色权限");
    });
};

/**
 * 重置过滤
 */
const handleResetPermissionFilter = (keyword: string) => {
  authorizeSearchKeywords[keyword] = undefined;
  handleGetRolePermissions();
};

/**
 * 提交角色基本信息表单
 */
const handleRoleInfoFormSubmit = (formData: any) => {
  updateRole(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetRoleDetail(roleId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新角色信息");
    });
};

/**
 * 重置角色基本信息表单
 */
const handleResetRoleInfoForm = () => {
  roleInfoFormRef.value.resetFields();
  handleGetRoleDetail(roleId.value);
};

/** 角色主体 */
const rolePrincipals = reactive([]);
let rolePrincipalsPagination;
/** 检索角色主体关键字 */
const searchRolePrincipalKeyword = ref("");

/**
 * 获取角色主体
 *
 * @param id 角色ID
 * @param page 页数
 * @param size 条数
 * @param keyword 用户名 / 用户组名检索关键字
 */
const handleGetRolePrincipals = (
  id: string = roleId.value,
  page: number = 1,
  size: number = 15
) => {
  getRolePrincipals(id, {
    page,
    size,
    keyword: searchRolePrincipalKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        rolePrincipals.length = 0;
        rolePrincipals.push(...data.list);

        rolePrincipalsPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取角色主体");
    });
};

/**
 * 检索角色主体
 */
const handleSearchRolePrincipal = () => {
  handleGetRolePrincipals();
};

/** 添加角色主体对话框 */
const addRolePrincipalModelVisible = ref(false);

/**
 * 打开添加角色主体对话框
 */
const handleOpenAddRolePrincipalModel = () => {
  handleGetAllUsers();
  handlGetAllUserGroups();
  addRolePrincipalModelVisible.value = true;
};

/**
 * 关闭添加角色主体对话框
 */
const handleCloseAddRolePrincipalModel = () => {
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
const activePrincipalTabKey = ref("1");

/** 所有用户集合 */
const allUsers = reactive([]);
/** 搜索用户关键字 */
const searchSelectUserKeyword = ref("");
const allUsersPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取全部用户
 *
 * @param page 页数
 * @param size 分页大小
 */
const handleGetAllUsers = (page: number = 1, size: number = 15) => {
  searchUser(searchSelectUserKeyword.value, {
    page,
    size,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page == 1) {
          allUsers.length = 0;
          allUsers.push(...data.list);
        } else {
          allUsers.push(...data.list);
        }
        allUsersPagination.current = data.current;
        allUsersPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户列表");
    });
};

/**
 * 搜索用户
 */
const handleSearchUser = () => {
  handleGetAllUsers(1);
};

/**
 * 滚动加载更多用户
 */
const allUsersContainerRef = ref();
let loadMoreUsersLoading = false;
const handleAllUsersContainerScroll = () => {
  if (loadMoreUsersLoading) return;
  const container = allUsersContainerRef.value;
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
const allUserGroups = reactive([]);
const searchSelectUserGroupKeyword = ref("");
const allUserGroupsPagination = {
  current: 1,
  total: 0,
};

/**
 * 获取所有用户组
 */
const handlGetAllUserGroups = (page: number = 1, size: number = 15) => {
  getUserGroupList({
    page,
    size,
    keyword: searchSelectUserGroupKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        if (page == 1) {
          allUserGroups.length = 0;
          allUserGroups.push(...data.list);
        } else {
          allUserGroups.push(...data.list);
        }
        allUserGroupsPagination.current = data.current;
        allUserGroupsPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户组列表");
    });
};

/**
 * 搜索用户组
 */
const handleSearchUserGroup = () => {
  handlGetAllUserGroups(1);
};

/**
 * 滚动加载更多用户组
 */
const allUserGroupsContainerRef = ref();
let loadMoreUserGroupsLoading = false;
const handleAllUserGroupsContainerScroll = () => {
  if (loadMoreUserGroupsLoading) return;
  const container = allUserGroupsContainerRef.value;
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
const addRolePrincipalsForm = reactive({
  userIds: [],
  userGroupIds: [],
});

/** 选择用户 / 用户组全选（半选） */
const selectUserCheckAll = ref(false);
const selectUserGroupCheckAll = ref(false);
const selectUserIndeterminate = ref(false);
const selectUserGroupIndeterminate = ref(false);

const handleChangeSelectUserCheckAll = (value) => {
  selectUserIndeterminate.value = false;
  // 全选
  if (value) {
    selectUserCheckAll.value = true;
    addRolePrincipalsForm.userIds.length = 0;
    allUsers.forEach((item) => {
      addRolePrincipalsForm.userIds.push(item.userId);
    });
  } else {
    selectUserCheckAll.value = false;
    addRolePrincipalsForm.userIds = [];
  }
};

/**
 * 选择用户变化
 */
const handleSelectUserChange = (selectedUsers) => {
  // 半选状态变化
  if (selectedUsers.length == 0) {
    selectUserIndeterminate.value = false;
  }
  if (selectedUsers.length > 0 && selectedUsers.length < allUsers.length) {
    selectUserIndeterminate.value = true;
  }
};

const handleChangeSelectUserGroupCheckAll = (value) => {
  selectUserGroupIndeterminate.value = false;
  // 全选
  if (value) {
    selectUserGroupCheckAll.value = true;
    addRolePrincipalsForm.userGroupIds.length = 0;
    allUserGroups.forEach((item) => {
      addRolePrincipalsForm.userGroupIds.push(item.id);
    });
  } else {
    selectUserGroupCheckAll.value = false;
    addRolePrincipalsForm.userGroupIds = [];
  }
};

/**
 * 选择用户组变化
 */
const handleSelectUserGroupChange = (selectedUserGroups) => {
  // 半选状态变化
  if (selectedUserGroups.length == 0) {
    selectUserGroupIndeterminate.value = false;
  }
  if (
    selectedUserGroups.length > 0 &&
    selectedUserGroups.length < allUserGroups.length
  ) {
    selectUserGroupIndeterminate.value = true;
  }
};

/** 已选择的角色主体 */
const selectedPrincipals = computed(() => {
  const principals = [];
  principals.push(
    ...allUsers.filter((item) =>
      addRolePrincipalsForm.userIds.includes(item.userId)
    )
  );
  principals.push(
    ...allUserGroups.filter((item) =>
      addRolePrincipalsForm.userGroupIds.includes(item.id)
    )
  );
  return principals;
});

/**
 * 移除已选择的主体
 */
const handleRemoveSelectedPrincipal = (principal: any) => {
  if (principal.userId) {
    const targetIndex = addRolePrincipalsForm.userIds.findIndex(
      (item) => item === principal.userId
    );
    addRolePrincipalsForm.userIds.splice(targetIndex, 1);
  }

  if (principal.id) {
    const targetIndex = addRolePrincipalsForm.userGroupIds.findIndex(
      (item) => item === principal.id
    );
    addRolePrincipalsForm.userGroupIds.splice(targetIndex, 1);
  }

  // 全选和半选状态变化
  if (addRolePrincipalsForm.userIds.length == 0) {
    selectUserIndeterminate.value = false;
    selectUserCheckAll.value = false;
  }

  if (
    addRolePrincipalsForm.userIds.length > 0 &&
    addRolePrincipalsForm.userIds.length < allUsers.length
  ) {
    selectUserIndeterminate.value = true;
  }

  if (addRolePrincipalsForm.userGroupIds.length == 0) {
    selectUserGroupIndeterminate.value = false;
    selectUserGroupCheckAll.value = false;
  }

  if (
    addRolePrincipalsForm.userGroupIds.length > 0 &&
    addRolePrincipalsForm.userGroupIds.length < allUserGroups.length
  ) {
    selectUserGroupIndeterminate.value = true;
  }
};

/**
 * 清空已选择的主体
 */
const handleClearSelctedPrincipals = () => {
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
const addRolePrincipalsFormSubmitLoading = ref(false);
const handleAddRolePrincipalsFormSubmit = () => {
  addRolePrincipalsFormSubmitLoading.value = true;
  addRoleMapping({
    roleIds: [roleId.value],
    ...addRolePrincipalsForm,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("添加成功");
        handleCloseAddRolePrincipalModel();
        handleGetRolePrincipals(roleId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "添加角色主体");
    })
    .finally(() => {
      addRolePrincipalsFormSubmitLoading.value = false;
    });
};

/**
 * 移除角色主体
 */
const handleRemoveRolePrincipal = (principal: any) => {
  Modal.warning({
    title: `确定移除主体「${principal.principal}」吗？`,
    content: "此操作不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeRoleMapping({
        roleIds: [roleId.value],
        userIds: [principal.principalId],
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("移除成功");
            handleGetRolePrincipals(roleId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "移除角色主体");
        });
    },
  });
};

/**
 * 取消授权
 */
const handleCancelAuthorization = (permission: any) => {
  Modal.warning({
    title: `确定取消对权限「${permission.permissionName}」的授权吗？`,
    content: "此操作不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      cancelAuthorization(permission.permissionId, roleId.value)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("取消授权成功");
            handleGetRoleDetail(roleId.value);
          });
        })
        .catch((err: any) => {
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
const handleToPrincipalDetail = (principal: any) => {
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
const hantoToUserGroupDetail = (id: string) => {
  router.push({
    path: "/user/group/detail",
    query: {
      id,
      active_tab: "user_group_info",
    },
  });
};

/**
 * 跳转用户详情
 *
 * @param user 用户信息
 */
const handleToUserDetail = (id: string) => {
  router.push({
    path: "/user/detail",
    query: {
      id,
      active_tab: "user_info",
    },
  });
};

/** 授权对话框 */
const authorizeVisible = ref(false);

/**
 * 授权
 */
const handleAuthorize = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.authorizeOptions.principal = roleName.value;
  globalVariables.authorizeOptions.principalId = roleId.value;
  globalVariables.authorizeOptions.principalType = "ROLE";

  authorizeVisible.value = true;
};

/**
 * 跳转资源组详情
 */
const handleToResourceGroupDetail = (id: string) => {
  router.push({
    path: "/resource/group/detail",
    query: {
      id,
      active_tab: "resource_group_info",
    },
  });
};

/**
 * 跳转资源详情
 */
const handleToResourceDetail = (id: string) => {
  router.push({
    path: "/permission/resource/detail",
    query: {
      id,
      active_tab: "resource_info",
    },
  });
};

/**
 * 跳转权限详情
 */
const handleToPermissionDetail = (id: string) => {
  router.push({
    path: "/permission/detail",
    query: {
      id,
      active_tab: "permission_info",
    },
  });
};

export default defineComponent({
  setup() {
    const roleId = getQueryString("id");
    const tab = getQueryString("active_tab");
    rolePrincipalsPagination = usePagination(
      `${roleId}_rolePrincipals`,
      ({ page, size }) => {
        if (tab === "role_info") {
          handleGetRolePrincipals(roleId, page, size);
        }
      }
    );
    permissionsPagination = usePagination(
      `${roleId}_rolePermissions`,
      ({ page, size }) => {
        if (tab === "permission_management") {
          handleGetRolePermissions(roleId, page, size);
        }
      }
    );

    onMounted(() => {
      activeTab.value = tab || "role_info";
      handleGetRoleDetail(roleId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      roleId,
      roleName,
      roleInfoFormRef,
      roleInfoForm,
      roleInfoFormRules,
      handleRoleInfoFormSubmit,
      handleResetRoleInfoForm,
      rolePrincipals,
      rolePrincipalsPagination,
      searchRolePrincipalKeyword,
      handleSearchRolePrincipal,
      addRolePrincipalModelVisible,
      handleOpenAddRolePrincipalModel,
      handleCloseAddRolePrincipalModel,
      allUsers,
      allUsersContainerRef,
      handleAllUsersContainerScroll,
      searchSelectUserKeyword,
      handleSearchUser,
      allUserGroups,
      allUserGroupsContainerRef,
      handleAllUserGroupsContainerScroll,
      searchSelectUserGroupKeyword,
      handleSearchUserGroup,
      addRolePrincipalsForm,
      selectUserCheckAll,
      selectUserGroupCheckAll,
      selectUserIndeterminate,
      selectUserGroupIndeterminate,
      handleChangeSelectUserCheckAll,
      handleChangeSelectUserGroupCheckAll,
      handleSelectUserChange,
      handleSelectUserGroupChange,
      selectedPrincipals,
      handleRemoveSelectedPrincipal,
      handleClearSelctedPrincipals,
      activePrincipalTabKey,
      addRolePrincipalsFormSubmitLoading,
      handleAddRolePrincipalsFormSubmit,
      handleRemoveRolePrincipal,
      permissions,
      handleCancelAuthorization,
      handleToPrincipalDetail,
      authorizeVisible,
      handleAuthorize,
      handleToResourceGroupDetail,
      handleToResourceDetail,
      handleToPermissionDetail,
      permissionsPagination,
      authorizeSearchKeywords,
      handleGetRolePermissions,
      resourceGroupNameFilter,
      resourceNameFilter,
      permissionNameFilter,
      permissionCodeFilter,
      handleResetPermissionFilter,
    };
  },
});
