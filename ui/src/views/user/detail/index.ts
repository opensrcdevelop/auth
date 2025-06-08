import {computed, defineComponent, h, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {
    clearAuthorizedTokens,
    clearAuthorizedTokensByLoginId,
    getUserAttrs,
    getUserDetail,
    getUserLoginLogs,
    getUserPermissions,
    rebindMfaDevice,
    updateUser,
} from "@/api/user";
import {generateRandomString, getQueryString, handleApiError, handleApiSuccess,} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {addRoleMapping, getRoleList, removeRoleMapping} from "@/api/role";
import {addUserGroupMapping, getUserGroupList, removeUserGroupMapping,} from "@/api/userGroup";
import {cancelAuthorization} from "@/api/permission";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import IconSearch from "@arco-design/web-vue/es/icon/icon-search";
import {getEnabledDictData} from "@/api/dict";
import {usePagination} from "@/hooks/usePagination";
import {checkPasswordWithoutPolicy} from "@/api/setting";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("user_info");

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

  if (tabKey === "permission_management") {
    handleGetUserPermissions();
  }

  if (tabKey === "login_logs") {
    handleGetUserLoginLogs();
  }
};

const userId = ref("");
const username = ref("");

/** 账号信息 */
const accountInfoForm = reactive({
  userId: "",
  createTime: "",
  lastLoginTime: "",
  lastLoginIp: "",
  lastLoginDeviceType: "",
  lastLoginDeviceOs: "",
});

/** 个人信息 */
const userInfoForm = reactive({
  userId: "",
  username: "",
  phoneNumber: "",
  emailAddress: "",
});
const userInfoFormRef = ref();

/** 所有用户扩展属性 */
const allUserExtAttrs = reactive([]);
const userAttrs = reactive([]);

/** 字典数据值 */
const allDictDatas = reactive({});

/** 用户角色 */
const userRoles = reactive([]);
/** 所属用户组 */
const userGroups = reactive([]);

/** 账户禁用状态 */
const accountLocked = ref(false);

/** MFA（多因素认证）状态 */
const enableMfa = ref(false);

/** 控制台访问状态 */
const consoleAccess = ref(false);

/**
 * 获取用户详情
 *
 * @param id 用户ID
 */
const handleGetUserDetail = (id: string) => {
  getUserDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
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
        userAttrs.push(...data.attributes);

        userRoles.length = 0;
        userRoles.push(...data.roles);

        userGroups.length = 0;
        userGroups.push(...data.userGroups);

        resetPwdForm.rawEmail = data.emailAddress;
        resetPwdForm.userId = data.id;

        accountLocked.value = data.locked;

        enableMfa.value = data.enableMfa;

        consoleAccess.value = data.consoleAccess;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户详情");
    });
};

/** 用户权限 */
let permissionsPagination;
const permissions = reactive([]);
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
    handleGetUserPermissions();
  },
  slotName: "resource-group-name-filter",
  icon: () => h(IconSearch),
};
// 资源名称过滤
const resourceNameFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.resourceName = value;
    handleGetUserPermissions();
  },
  slotName: "resource-name-filter",
  icon: () => h(IconSearch),
};
// 资源名称过滤
const permissionNameFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.permissionName = value;
    handleGetUserPermissions();
  },
  slotName: "permission-name-filter",
  icon: () => h(IconSearch),
};
// 资源标识过滤
const permissionCodeFilter = {
  filter: (value, record) => {
    authorizeSearchKeywords.permissionCode = value;
    handleGetUserPermissions();
  },
  slotName: "permission-code-filter",
  icon: () => h(IconSearch),
};

/**
 * 获取用户权限
 */
const handleGetUserPermissions = (
  id: string = userId.value,
  page: number = 1,
  size: number = 15
) => {
  getUserPermissions(id, {
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

        permissionsPagination.updatePagination(data.current, data.total, data.size);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户权限");
    });
};

/**
 * 重置过滤
 */
const handleResetPermissionFilter = (keyword: string) => {
  authorizeSearchKeywords[keyword] = undefined;
  handleGetUserPermissions();
};

/**
 * 获取全部用户扩展属性
 */
const handleGetUserExtAttrs = () => {
  getUserAttrs({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        allUserExtAttrs.length = 0;
        data.list.forEach((item: any) => {
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
    .catch((err: any) => {
      handleApiError(err, "获取用户扩展属性");
    });
};

/**
 * 获取启用的字典数据
 */
const handleGetEnabledDictData = async (attrKey: string, dictId: string) => {
  try {
    const result = await getEnabledDictData(dictId);
    handleApiSuccess(result, (data: any) => {
      allDictDatas[attrKey].length = 0;
      allDictDatas[attrKey].push(...data);
    });
  } catch (err: any) {
    handleApiError(err, "获取启用的字典数据");
  }
};

/**
 * 用户扩展属性值
 */
const userAttrValues = computed(() => {
  const userAttrValues = [];
  allUserExtAttrs.map((item: any) => {
    const userAttr = userAttrs.find((attr: any) => attr.key === item.key);
    if (userAttr) {
      userAttrValues.push(userAttr.value);
    } else {
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
const handleSetAccountStatus = (locked: boolean) => {
  updateUser({
    userId: userId.value,
    locked,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success(locked ? "禁用成功" : "启用成功");
        handleGetUserDetail(userId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新用户信息");
    });
};

/**
 * 设置 MFA 状态
 *
 * @param enableMfa MFA 状态
 */
const handleSetMfaStatus = (enableMfa: boolean) => {
  updateUser({
    userId: userId.value,
    enableMfa,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success(enableMfa ? "启用 MFA 成功" : "关闭 MFA 成功");
        handleGetUserDetail(userId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新用户信息");
    });
};

/**
 * 重新绑定 MFA 设备
 */
const handleRebindMfaDevice = () => {
  rebindMfaDevice(userId.value)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("重新绑定 MFA 设备成功");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "重新绑定 MFA 设备");
    });
};

/**
 * 设置控制台访问状态
 *
 * @param consoleAccess 控制台访问状态
 */
const handleSetConsoleAccessStatus = (consoleAccess: boolean) => {
  updateUser({
    userId: userId.value,
    consoleAccess,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success(
          consoleAccess ? "开启控制台访问成功" : "关闭控制台访问成功"
        );
        handleGetUserDetail(userId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新用户信息");
    });
};

/**
 * 清除授权
 */
const handleClearAuthorizedTokens = () => {
  clearAuthorizedTokens(userId.value)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("清除授权的 Token 成功");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "清除授权的 Token");
    });
};

/**
 * 提交个人信息表单
 *
 * @param formData 个人信息表单
 */
const handleUserInfoFormSubmit = (formData: any) => {
  updateUser(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetUserDetail(userId.value);
        handleGetUserExtAttrs();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新用户信息");
    });
};

/**
 * 提交用户扩展属性表单
 */
const handleUserAttrsSubmit = () => {
  const userAttrs = [];
  allUserExtAttrs.forEach((item: any, index: number) => {
    userAttrs.push({
      attrId: item.id,
      attrValue: userAttrValues.value[index],
    });
  });
  updateUser({
    userId: userId.value,
    attributes: userAttrs,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetUserDetail(userId.value);
        handleGetUserExtAttrs();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新用户信息");
    });
};

/**
 * 重置个人信息表单
 */
const handleResetUserInfoForm = () => {
  userInfoFormRef.value.resetFields();
  handleGetUserDetail(userId.value);
  handleGetUserExtAttrs();
};

/**
 * 重置用户扩展属性
 */
const handleResetUserAttrs = () => {
  handleGetUserDetail(userId.value);
  handleGetUserExtAttrs();
};

/**
 * 撤销用户角色
 *
 * @param role 角色
 */
const handleRemoveUserRole = (role: any) => {
  Modal.warning({
    title: `确定撤销角色「${role.name}」吗？`,
    content: `撤销后该用户将失去「${role.name}」所拥有的权限。`,
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeRoleMapping({
        userIds: [userId.value],
        roleIds: [role.id],
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("撤销成功");
            handleGetUserDetail(userId.value);
          });
        })
        .catch((err: any) => {
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
const handleRemoveUserGroup = (group: any) => {
  Modal.warning({
    title: `确定移除用户组「${group.name}」吗？`,
    content: "此操作不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeUserGroupMapping({
        userIds: [userId.value],
        userGroupIds: [group.id],
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("移除成功");
            handleGetUserDetail(userId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "移除用户组");
        });
    },
  });
};

const roleList = reactive([]);
const roleListPagination = {
  current: 1,
  total: 0,
};
const roleSearchKeyword = ref("");
const addUserRoleModalVisible = ref(false);

const addUserRoleFormRef = ref();
const addUserRoleForm = reactive({
  roleIds: [],
});
const addUserRoleFormRules = {
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
 * 打开添加用户角色对话框
 */
const handleOpenAddUserRoleModal = () => {
  handleGetRoleList();
  addUserRoleModalVisible.value = true;
};

/**
 * 关闭添加用户角色对话框
 */
const handleCloseAddUserRoleModal = () => {
  addUserRoleFormRef.value.resetFields();
  addUserRoleModalVisible.value = false;
};

/**
 * 提交添加用户角色表单
 *
 * @param formData 添加用户角色表单
 */
const addUserRoleFormSubmitLoading = ref(false);
const handleAddUserRoleFormSubmit = (formData: any) => {
  addUserRoleFormSubmitLoading.value = true;
  addRoleMapping({
    userIds: [userId.value],
    roleIds: formData.roleIds,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("添加成功");
        handleCloseAddUserRoleModal();
        handleGetUserDetail(userId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "添加角色");
    })
    .finally(() => {
      addUserRoleFormSubmitLoading.value = false;
    });
};

/** 用户组列表 */
const userGroupList = reactive([]);
const userGroupListPagination = {
  current: 1,
  total: 0,
};
const userGroupSearchKeyword = ref("");
const addUserGroupModalVisible = ref(false);

const addUserGroupFormRef = ref();
const addUserGroupForm = reactive({
  userGroupIds: [],
});
const addUserGroupFormRules = {
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

/**
 * 打开添加用户组对话框
 */
const handleOpenAddUserGroupModal = () => {
  handleGetUserGroupList();
  addUserGroupModalVisible.value = true;
};

/**
 * 关闭添加用户组对话框
 */
const handleCloseAddUserGroupModal = () => {
  addUserGroupFormRef.value.resetFields();
  addUserGroupModalVisible.value = false;
};

/**
 * 提交添加用户组表单
 *
 * @param formData 用户组表单
 */
const addUserGroupFormSubmitLoading = ref(false);
const handleAddUserGroupFormSubmit = (formData: any) => {
  addUserGroupFormSubmitLoading.value = true;
  addUserGroupMapping({
    userIds: [userId.value],
    userGroupIds: formData.userGroupIds,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("添加成功");
        handleCloseAddUserGroupModal();
        handleGetUserDetail(userId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "添加用户组");
    })
    .finally(() => {
      addUserGroupFormSubmitLoading.value = false;
    });
};

/** 重置密码对话框 */
const resetPwdModalVisible = ref(false);

const resetPwdForm = reactive({
  userId: "",
  password: "",
  rawEmail: "",
  emailAddress: "",
  needChangePwd: true,
  sendEmail: true,
});

const resetPwdFormRules = {
  password: [
    {
      required: true,
      message: "密码未填写",
    },
  ],
  emailAddress: [
    {
      validator: (value, cb) => {
        if (resetPwdForm.sendEmail && !resetPwdForm.rawEmail && !value) {
          cb("邮箱未填写");
        } else {
          cb();
        }
      },
    },
  ],
};
const resetPwdFormRef = ref();

const resetPwdFormSubmitLoading = ref(false);

/** 打开重置密码对话框 */
const handleOpenResetPwdModal = () => {
  resetPwdModalVisible.value = true;
};

/** 关闭重置密码对话框 */
const handleCloseResetPwdModal = () => {
  resetPwdFormRef.value.resetFields();
  passwordCheckerRef.value.setPassword("");
  resetPwdModalVisible.value = false;
};

/**
 * 生成随机密码
 */
const handleGeneratePassword = () => {
  passwordCheckerRef.value.setPassword(generateRandomString(12));
};

/**
 * 提交重置密码表单
 */
const handleResetPwdFormSubmit = () => {
  if (!checkPasswordRes.valid) {
    return;
  }

  if (resetPwdForm.rawEmail && !resetPwdForm.emailAddress) {
    resetPwdForm.emailAddress = resetPwdForm.rawEmail;
  } else {
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
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("重置密码成功");
        handleCloseResetPwdModal();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "重置密码");
    })
    .finally(() => {
      resetPwdFormSubmitLoading.value = false;
    });
};

/**
 * 密码检查
 */
const passwordCheckerRef = ref(null);
const checkPasswordLoading = ref(false);
const checkPasswordRes = reactive({
  valid: false,
  errorMessage: undefined,
  ruleResults: undefined,
});
const handleCheckPassword = (password: string) => {
  checkPasswordLoading.value = true;
  resetPwdForm.password = password;
  checkPasswordWithoutPolicy({
    identity: resetPwdForm.userId,
    password,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        checkPasswordRes.valid = data.valid;
        checkPasswordRes.errorMessage = data.errorMessage;
        if (data.ruleResults) {
          checkPasswordRes.ruleResults = data.ruleResults;
        } else {
          checkPasswordRes.ruleResults = [];
        }
        checkPasswordLoading.value = false;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "密码检查");
      checkPasswordLoading.value = false;
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
      cancelAuthorization(permission.permissionId, userId.value)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("取消授权成功");
            handleGetUserDetail(userId.value);
          });
        })
        .catch((err: any) => {
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
const handleToRoleDetail = (role: any) => {
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
const hantoToUserGroupDetail = (userGroup: any) => {
  router.push({
    path: "/user/group/detail",
    query: {
      id: userGroup.id,
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
  globalVariables.authorizeOptions.principal = username.value;
  globalVariables.authorizeOptions.principalId = userId.value;
  globalVariables.authorizeOptions.principalType = "USER";

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

/** 登录日志 */
const loginLogs = reactive([]);
const loginLogsPagination = reactive({
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
const handleGetUserLoginLogs = (
  id: string = userId.value,
  page: number = 1,
  size: number = 15
) => {
  getUserLoginLogs(id, {
    page,
    size,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        loginLogs.length = 0;
        loginLogs.push(...data.list);

        loginLogsPagination.total = data.total;
        loginLogsPagination.current = data.current;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户登录日志");
    });
};

/**
 * 用户登录日志页数变化
 *
 * @param page 页数
 */
const handleLoginLogsPageChange = (page: number) => {
  loginLogsPagination.current = page;
  handleGetUserLoginLogs(userId.value, page, permissionsPagination.pageSize);
};

/**
 * 用户登录日志分页大小变化
 *
 * @param page 分页大小
 */
const handleLoginLogsPageSizeChange = (size: number) => {
  loginLogsPagination.pageSize = size;
  handleGetUserLoginLogs(userId.value, 1, size);
};

/**
 * 清除登录 ID 关联的 Token
 */
const handleClearAuthorizedTokensByLoginId = (loginLog: any) => {
  clearAuthorizedTokensByLoginId(loginLog.loginId)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("清除本次登录授权的 Token 成功");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "清除本次登录授权的 Token");
    });
};

export default defineComponent({
  setup() {
    const userId = getQueryString("id");
    permissionsPagination = usePagination(
      `${userId}_userPermissions`,
      ({ page, size }) => {
        if (getQueryString("active_tab") === "permission_management") {
          handleGetUserPermissions(userId, page, size);
        }
      }
    );

    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "user_info";
      handleGetUserDetail(userId);
      handleGetUserExtAttrs();
      if (activeTab.value === "login_logs") {
        handleGetUserLoginLogs(userId);
      }
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      userId,
      username,
      userInfoForm,
      accountInfoForm,
      allUserExtAttrs,
      userAttrValues,
      handleUserInfoFormSubmit,
      handleUserAttrsSubmit,
      userInfoFormRef,
      handleResetUserInfoForm,
      handleResetUserAttrs,
      userRoles,
      userGroups,
      permissions,
      handleRemoveUserRole,
      handleRemoveUserGroup,
      addUserRoleModalVisible,
      handleOpenAddUserRoleModal,
      addUserRoleForm,
      roleList,
      roleSearchKeyword,
      loadMoreRole,
      handleSearchRole,
      addUserRoleFormRules,
      addUserRoleFormRef,
      handleCloseAddUserRoleModal,
      addUserRoleFormSubmitLoading,
      handleAddUserRoleFormSubmit,
      userGroupList,
      userGroupSearchKeyword,
      loadMoreUserGroup,
      handleSearchUserGroup,
      addUserGroupModalVisible,
      handleOpenAddUserGroupModal,
      handleCloseAddUserGroupModal,
      addUserGroupForm,
      addUserGroupFormRules,
      addUserGroupFormRef,
      addUserGroupFormSubmitLoading,
      handleAddUserGroupFormSubmit,
      handleOpenResetPwdModal,
      handleCloseResetPwdModal,
      resetPwdModalVisible,
      resetPwdFormRef,
      resetPwdForm,
      resetPwdFormRules,
      handleGeneratePassword,
      handleResetPwdFormSubmit,
      resetPwdFormSubmitLoading,
      handleCancelAuthorization,
      handleToRoleDetail,
      hantoToUserGroupDetail,
      authorizeVisible,
      handleAuthorize,
      handleToResourceGroupDetail,
      handleToResourceDetail,
      handleToPermissionDetail,
      accountLocked,
      enableMfa,
      handleSetAccountStatus,
      handleSetMfaStatus,
      handleRebindMfaDevice,
      handleClearAuthorizedTokens,
      consoleAccess,
      handleSetConsoleAccessStatus,
      permissionsPagination,
      authorizeSearchKeywords,
      handleGetUserPermissions,
      resourceGroupNameFilter,
      resourceNameFilter,
      permissionNameFilter,
      permissionCodeFilter,
      handleResetPermissionFilter,
      allDictDatas,
      loginLogs,
      loginLogsPagination,
      handleLoginLogsPageChange,
      handleLoginLogsPageSizeChange,
      handleClearAuthorizedTokensByLoginId,
      passwordCheckerRef,
      checkPasswordLoading,
      checkPasswordRes,
      handleCheckPassword
    };
  },
});
