import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { useRoute } from "vue-router";
import {
  addAuthorizeCondition,
  cancelAuthorization,
  getPermissionDetail,
  getPermissionExpList,
  removeAuthorizeCondition,
  updatePermission,
} from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("permission_info");

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


const permissionId = ref("");
const permissionName = ref("");

/** 权限基本信息 */
const permissionInfoFormRef = ref();
const permissionInfoForm = reactive({
  id: undefined,
  code: undefined,
  name: undefined,
  desc: undefined,
  resourceId: undefined,
});
const permissionInfoFormRules = {
  name: [{ required: true, message: "权限名称未填写" }],
  code: [
    { required: true, message: "权限编码未填写" },
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
 * 提交权限基本信息表单
 *
 * @param formData 权限基本信息表单
 */
const permissionInfoFormSubmit = (formData: any) => {
  updatePermission(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetPermissionDetail(permissionId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新权限");
    });
};

/**
 * 重置权限基本信息表单
 */
const handleResetPermissionInfoForm = () => {
  permissionInfoFormRef.value.resetFields();
  handleGetPermissionDetail(permissionId.value);
};

/** 授权记录 */
const authorizeRecords = reactive([]);

/**
 * 获取权限详情
 *
 * @param id 权限ID
 */
const handleGetPermissionDetail = (id: string) => {
  getPermissionDetail(id, {
    keyword: authorizedPrincipalSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissionId.value = data.permissionId;
        permissionName.value = data.permissionName;

        permissionInfoForm.id = data.permissionId;
        permissionInfoForm.code = data.permissionCode;
        permissionInfoForm.name = data.permissionName;
        permissionInfoForm.desc = data.permissionDesc;
        permissionInfoForm.resourceId = data.resourceId;

        authorizeRecords.length = 0;
        authorizeRecords.push(...data.authorizeRecords);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限详情");
    });
};

/** 被授权主体搜索关键字 */
const authorizedPrincipalSearchKeyword = ref("");

/**
 * 搜索被授权主体
 */
const handleSerachAuthorizedPrincipal = () => {
  handleGetPermissionDetail(permissionId.value);
};

/**
 * 跳转被授权主体详情
 */
const handeToPrincipalDetail = (principal: any) => {
  if (principal.principalType === "USER") {
    handleToUserDetail(principal.principalId);
  }

  if (principal.principalType === "USER_GROUP") {
    hantoToUserGroupDetail(principal.principalId);
  }

  if (principal.principalType === "ROLE") {
    handleToRoleDetail(principal.principalId);
  }
};

/**
 * 跳转用户组详情
 */
const hantoToUserGroupDetail = (id: string) => {
  router.push({
    path: "/user/group/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转用户详情
 */
const handleToUserDetail = (id: string) => {
  router.push({
    path: "/user/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转角色详情
 */
const handleToRoleDetail = (id: string) => {
  router.push({
    path: "/role/detail",
    query: {
      id,
    },
  });
};

/** 添加限制条件对话框 */
const addAuthorizeConditionModalVisible = ref(false);
const addAuthorizeConditionFormRef = ref();
const addAuthorizeConditionForm = reactive({
  authorizeIds: undefined,
  permissionExpIds: undefined,
});
const addAuthorizeConditionFormRules = {
  permissionExpIds: [{ required: true, message: "请选择限制条件" }],
};

/** 限制条件列表 */
const authorizeConditionList = reactive([]);
const authorizeConditionListPagination = {
  total: 0,
  current: 1,
};
const authorizeConditionSearchKeyword = ref("");

/**
 * 打开添加限制条件对话框
 */
const handleOpenAddAuthorizeConditionModal = (authorizeRecord: any) => {
  addAuthorizeConditionModalVisible.value = true;
  addAuthorizeConditionForm.authorizeIds = [authorizeRecord.authorizeId];
  handleGetAuthorizeConditionList();
};

/**
 * 关闭添加限制条件对话框
 */
const handleCloseAddAuthorizeConditionModal = () => {
  addAuthorizeConditionModalVisible.value = false;
  addAuthorizeConditionFormRef.value.resetFields();
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

let loadMoreAuthorizeConditionLoading = false;
/**
 * 加载更多限制条件
 */
const loadMoreAuthorizeCondition = () => {
  if (loadMoreAuthorizeConditionLoading) return;
  if (authorizeConditionList.length < authorizeConditionListPagination.total) {
    loadMoreAuthorizeConditionLoading = true;
    authorizeConditionListPagination.current++;
    handleGetAuthorizeConditionList(authorizeConditionListPagination.current);
    loadMoreAuthorizeConditionLoading = false;
  }
};

/**
 * 搜索限制条件
 */
const handleSearchAuthorizeCondition = () => {
  handleGetAuthorizeConditionList(1);
};

const addAuthorizeConditionFormSubmitLoading = ref(false);
/**
 * 添加限制条件表单提交
 */
const handleAddAuthorizeConditionFormSubmit = () => {
  addAuthorizeConditionFormSubmitLoading.value = true;
  addAuthorizeCondition(addAuthorizeConditionForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        handleCloseAddAuthorizeConditionModal();
        handleGetPermissionDetail(permissionId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "添加限制条件");
    })
    .finally(() => {
      addAuthorizeConditionFormSubmitLoading.value = false;
    });
};

/**
 * 取消限制
 */
const handleRemoveAuthorizeCondition = (
  authoruzeRecord: any,
  permissionExp: any
) => {
  Modal.confirm({
    title: `确定取消限制条件「${permissionExp.name}」的限制吗？`,
    content: "此操作将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeAuthorizeCondition({
        authorizeIds: [authoruzeRecord.authorizeId],
        permissionExpIds: [permissionExp.id],
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            handleGetPermissionDetail(permissionId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "取消限制");
        });
    },
  });
};

/**
 * 取消授权
 */
const handleCancelAuthorization = (principalId: string) => {
  Modal.warning({
    title: `确定取消对权限「${permissionName.value}」的授权吗？`,
    content: "此操作不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      cancelAuthorization(permissionId.value, principalId)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("取消授权成功");
            handleGetPermissionDetail(permissionId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "取消授权");
        });
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
      const permissionId = route.query.id as string;
      handleGetPermissionDetail(permissionId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      permissionId,
      permissionName,
      permissionInfoFormRef,
      permissionInfoForm,
      permissionInfoFormRules,
      authorizedPrincipalSearchKeyword,
      handleSerachAuthorizedPrincipal,
      authorizeRecords,
      handleResetPermissionInfoForm,
      permissionInfoFormSubmit,
      handeToPrincipalDetail,
      addAuthorizeConditionModalVisible,
      addAuthorizeConditionFormRef,
      addAuthorizeConditionForm,
      addAuthorizeConditionFormRules,
      handleOpenAddAuthorizeConditionModal,
      handleCloseAddAuthorizeConditionModal,
      authorizeConditionList,
      authorizeConditionListPagination,
      authorizeConditionSearchKeyword,
      handleSearchAuthorizeCondition,
      loadMoreAuthorizeCondition,
      addAuthorizeConditionFormSubmitLoading,
      handleAddAuthorizeConditionFormSubmit,
      handleRemoveAuthorizeCondition,
      handleCancelAuthorization
    };
  },
});
