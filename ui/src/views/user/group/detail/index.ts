import { computed, defineComponent, h, onMounted, reactive, ref } from "vue";
import router from "@/router";
import {
  addUserGroupMapping,
  getGroupUsers,
  getUserGroupDetail,
  getUserGroupPermissions,
  removeUserGroupMapping,
  updateUserGroup,
} from "@/api/userGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { useRoute } from "vue-router";
import { Modal, Notification } from "@arco-design/web-vue";
import { searchUser } from "@/api/user";
import { cancelAuthorization } from "@/api/permission";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import IconSearch from "@arco-design/web-vue/es/icon/icon-search";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("user_group_info");

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
};

const userGroupName = ref("");
const userGroupId = ref("");

/** 用户组基本信息表单 */
const userGroupInfoFormRef = ref();
const userGroupInfoForm = reactive({
  id: undefined,
  name: undefined,
  code: undefined,
  desc: undefined,
});
const userGroupInfoFormRules = {
  name: [{ required: true, message: "用户组名称未填写" }],
  code: [
    { required: true, message: "用户组标识未填写" },
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
 * 获取用户组详情
 *
 * @param id 用户组id
 */
const handleGetUserGroupDetail = (id: string) => {
  getUserGroupDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userGroupName.value = data.name;
        userGroupId.value = data.id;

        userGroupInfoForm.id = data.id;
        userGroupInfoForm.name = data.name;
        userGroupInfoForm.code = data.code;
        userGroupInfoForm.desc = data.desc;

        handleGetUserGroupPermissions();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户组详情");
    });
};

/** 用户组权限 */
const permissions = reactive([]);
const permissionsPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});
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
    handleGetUserGroupPermissions();
  },
  slotName: "resource-group-name-filter",
  icon: () => h(IconSearch),
};
// 资源名称过滤
const resourceNameFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.resourceName = value;
    handleGetUserGroupPermissions();
  },
  slotName: "resource-name-filter",
  icon: () => h(IconSearch),
};
// 资源名称过滤
const permissionNameFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.permissionName = value;
    handleGetUserGroupPermissions();
  },
  slotName: "permission-name-filter",
  icon: () => h(IconSearch),
};
// 资源标识过滤
const permissionCodeFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.permissionCode = value;
    handleGetUserGroupPermissions();
  },
  slotName: "permission-code-filter",
  icon: () => h(IconSearch),
};

/**
 * 获取用户组权限
 */
const handleGetUserGroupPermissions = (page: number = 1, size: number = 15) => {
  getUserGroupPermissions(userGroupId.value, {
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
        
        permissionsPagination.total = data.total;
        permissionsPagination.current = data.current;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户组权限");
    });
};

/**
 * 用户组权限页数变化
 *
 * @param page 页数
 */
const handlePermissionsPageChange = (page: number) => {
  permissionsPagination.current = page;
  handleGetUserGroupPermissions(page, permissionsPagination.pageSize);
};

/**
 * 用户组权限分页大小变化
 *
 * @param page 分页大小
 */
const handlePermissionsPageSizeChange = (size: number) => {
  permissionsPagination.pageSize = size;
  handleGetUserGroupPermissions(1, size);
};

/**
 * 重置过滤
 */
const handleResetPermissionFilter = (keyword: string) => {
  authorizeSearchKeywords[keyword] = undefined;
  handleGetUserGroupPermissions();
};

const groupUsers = reactive([]);
const groupUsersPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取组内用户
 *
 * @param id 用户组 ID
 * @param page 页数
 * @param size 条数
 * @param keyword 用户名 / 邮箱 / 手机号 搜索关键字
 */
const handleGetGroupUsers = (
  id: string,
  page: number = 1,
  size: number = 15,
  keyword: string = ""
) => {
  getGroupUsers(id, {
    page,
    size,
    keyword,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        groupUsers.length = 0;
        groupUsers.push(...data.list);

        groupUsersPagination.current = data.current;
        groupUsersPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取组内用户");
    });
};

/**
 * 提交用户组信息表单
 *
 * @param formData 用户组信息表单
 */
const handleUserGroupInfoFormSubmit = (formData: any) => {
  updateUserGroup(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetUserGroupDetail(userGroupId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新用户组信息");
    });
};

/**
 * 重置用户组信息表单
 */
const handleResetUserGroupInfoForm = () => {
  handleGetUserGroupDetail(userGroupId.value);
  userGroupInfoFormRef.value.resetFields();
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  groupUsersPagination.pageSize = size;
  handleGetGroupUsers(userGroupId.value, 1, size, searchGroupUserKeyword.value);
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  groupUsersPagination.current = page;
  handleGetGroupUsers(
    userGroupId.value,
    page,
    groupUsersPagination.pageSize,
    searchGroupUserKeyword.value
  );
};

/** 用户组成员搜索关键字 */
const searchGroupUserKeyword = ref("");
/**
 * 搜索用户组成员
 */
const handleSearchGroupUser = () => {
  handleGetGroupUsers(userGroupId.value, 1, 15, searchGroupUserKeyword.value);
};

const addGroupUserModalVisible = ref(false);
/**
 * 打开添加用户组成员对话框
 */
const handleOpenAddGroupUserModal = () => {
  addGroupUserModalVisible.value = true;
  handleGetAllUsers();
};

/**
 * 关闭添加用户组成员对话框
 */
const handleCloseAddGroupUserModal = () => {
  addGroupUserModalVisible.value = false;
  addGroupUsersForm.userIds = [];
  searchSelectUserKeyword.value = "";
  allUsers.length = 0;
  allUsersPagination.current = 1;
  selectUserIndeterminate.value = false;
  selectUserCheckAll.value = false;
};

/** 所有用户集合 */
const allUsers = reactive([]);
/** 搜索成员关键字 */
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
 * 搜索成员
 */
const handleSearchUser = () => {
  handleGetAllUsers(1);
};

/**
 * 滚动加载更多用户
 */
const allUsersContainerRef = ref(null);
let loadMoreUsersLoading = false;
const handleAllUsersContainerScroll = () => {
  const container = allUsersContainerRef.value;
  // 滚动到底部
  if (container.scrollTop + container.clientHeight >= container.scrollHeight) {
    // 有更多数据
    if (allUsers.length < allUsersPagination.total) {
      if (loadMoreUsersLoading) return;
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
const addGroupUsersForm = reactive({
  userIds: [],
});

/** 选择成员全选 */
const selectUserCheckAll = ref(false);
/** 选择成员半选 */
const selectUserIndeterminate = ref(false);

/**
 * 选择成员全选变化
 */
const handleChangeCheckAll = (value) => {
  selectUserIndeterminate.value = false;
  // 全选
  if (value) {
    selectUserCheckAll.value = true;
    addGroupUsersForm.userIds.length = 0;
    allUsers.forEach((item) => {
      addGroupUsersForm.userIds.push(item.userId);
    });
  } else {
    selectUserCheckAll.value = false;
    addGroupUsersForm.userIds = [];
  }
};

/** 选择成员变化 */
const handleSelectUserChange = (selectedUsers) => {
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
const slectedUsers = computed(() => {
  return allUsers.filter((item) =>
    addGroupUsersForm.userIds.includes(item.userId)
  );
});

/**
 * 移除已选择的成员
 */
const handleRemoveSelectedUser = (userId: string) => {
  const targetIndex = addGroupUsersForm.userIds.findIndex(
    (item) => item === userId
  );
  addGroupUsersForm.userIds.splice(targetIndex, 1);

  // 全选和半选状态变化
  if (addGroupUsersForm.userIds.length == 0) {
    selectUserIndeterminate.value = false;
    selectUserCheckAll.value = false;
  }

  if (
    addGroupUsersForm.userIds.length > 0 &&
    addGroupUsersForm.userIds.length < allUsers.length
  ) {
    selectUserIndeterminate.value = true;
  }
};

/**
 * 清空已选择的成员
 */
const handleClearSelctedUsers = () => {
  selectUserCheckAll.value = false;
  selectUserIndeterminate.value = false;
  addGroupUsersForm.userIds = [];
};

/**
 * 添加成员表单提交
 */
const addGroupUsersFormSubmitLoading = ref(false);
const handleAddGroupUsersFormSubmit = () => {
  if (addGroupUsersForm.userIds.length > 0) {
    addGroupUsersFormSubmitLoading.value = true;
    addUserGroupMapping({
      userGroupIds: [userGroupId.value],
      userIds: addGroupUsersForm.userIds,
    })
      .then((result: any) => {
        handleApiSuccess(result, () => {
          Notification.success("添加成员成功");
          handleCloseAddGroupUserModal();
          handleGetGroupUsers(userGroupId.value);
        });
      })
      .catch((err: any) => {
        handleApiError(err, "添加成员");
      })
      .finally(() => {
        addGroupUsersFormSubmitLoading.value = false;
      });
  }
};

/**
 * 移除用户组成员
 */
const handleRemoveGroupUser = (user: any) => {
  Modal.warning({
    title: `确定移除成员「${user.username}」吗？`,
    content: "此操作不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeUserGroupMapping({
        userGroupIds: [userGroupId.value],
        userIds: [user.id],
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("移除成功");
            handleGetGroupUsers(userGroupId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "移除成员");
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
      cancelAuthorization(permission.permissionId, userGroupId.value)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("取消授权成功");
            handleGetUserGroupDetail(userGroupId.value);
          });
        })
        .catch((err: any) => {
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
const handleToUserDetail = (user: any) => {
  router.push({
    path: "/user/detail",
    query: {
      id: user.id,
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
  globalVariables.authorizeOptions.principal = userGroupName.value;
  globalVariables.authorizeOptions.principalId = userGroupId.value;
  globalVariables.authorizeOptions.principalType = "USER_GROUP";

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
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
      const userGroupId = route.query.id as string;
      handleGetUserGroupDetail(userGroupId);
      handleGetGroupUsers(userGroupId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      userGroupName,
      userGroupId,
      groupUsers,
      groupUsersPagination,
      userGroupInfoForm,
      userGroupInfoFormRef,
      userGroupInfoFormRules,
      handleUserGroupInfoFormSubmit,
      handleResetUserGroupInfoForm,
      searchGroupUserKeyword,
      handleSearchGroupUser,
      handlePageSizeChange,
      handlePageChange,
      addGroupUserModalVisible,
      handleOpenAddGroupUserModal,
      handleCloseAddGroupUserModal,
      allUsers,
      selectUserCheckAll,
      selectUserIndeterminate,
      addGroupUsersForm,
      handleChangeCheckAll,
      handleSelectUserChange,
      slectedUsers,
      handleRemoveSelectedUser,
      searchSelectUserKeyword,
      handleGetAllUsers,
      handleSearchUser,
      allUsersContainerRef,
      handleAllUsersContainerScroll,
      handleClearSelctedUsers,
      addGroupUsersFormSubmitLoading,
      handleAddGroupUsersFormSubmit,
      handleRemoveGroupUser,
      permissions,
      handleCancelAuthorization,
      handleToUserDetail,
      authorizeVisible,
      handleAuthorize,
      handleToResourceGroupDetail,
      handleToResourceDetail,
      handleToPermissionDetail,
      permissionsPagination,
      authorizeSearchKeywords,
      handleGetUserGroupPermissions,
      handlePermissionsPageChange,
      handlePermissionsPageSizeChange,
      resourceGroupNameFilter,
      resourceNameFilter,
      permissionNameFilter,
      permissionCodeFilter,
      handleResetPermissionFilter,
    };
  },
});
