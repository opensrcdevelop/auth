import {defineComponent, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {
    getPermissionExpDetail,
    getPermissionExpPermissions,
    getPermissionExpTemplateList,
    getPremissionExpTemplateParamConfigs,
    removeAuthorizeCondition,
    updatePermissionExp,
} from "@/api/permission";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import ParamInput from "../components/ParamInput.vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("condition_info");

/**
 * tab 切换事件
 */
const handleTabChange = (tabKey: string) => {
  router.replace({
    query: {
      ...router.currentRoute.value.query,
      active_tab: tabKey,
    },
  });
  activeTab.value = tabKey;
  handleTabInit(tabKey);
};

/**
 * tab 初始化
 */
const handleTabInit = (tabKey: string, id: string = permissionExpId.value) => {
  switch (tabKey) {
    case "condition_info":
      handleGetPermissionExpDetail(id);
      break;
    case "permission_list":
      if (!permissionExpId.value) {
        handleGetPermissionExpDetail(id);
      }
      handleGetPermissionExpPermissions(id);
      break;
  }
};

/** 模板列表 */
const templateList = reactive([]);
const handleGetTemplateList = () => {
  getPermissionExpTemplateList({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        templateList.length = 0;
        templateList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取限制条件模板列表");
    });
};

const permissionExpId = ref("");
const permissionExpName = ref("");

/** 权限表达式表单 */
const templateParamsRef = ref(null);
const permissionExpInfoFormRef = ref();
const permissionExpInfoForm = reactive({
  id: undefined,
  name: undefined,
  templateId: undefined,
  templateParams: undefined,
  expression: undefined,
  desc: undefined,
});
const permissionExpInfoFormRules = {
  name: [{ required: true, message: "限制条件名称未填写" }],
  expression: [{ required: true, message: "JEXL 表达式未填写" }],
};

/**
 * 获取权限表达式详情
 */
const handleGetPermissionExpDetail = async (id: string) => {
  try {
    const result = await getPermissionExpDetail(id);
    handleApiSuccess(result, (data: any) => {
      permissionExpId.value = data.id;
      permissionExpName.value = data.name;

      permissionExpInfoForm.id = data.id;
      permissionExpInfoForm.name = data.name;
      permissionExpInfoForm.templateId = data.templateId;
      permissionExpInfoForm.templateParams = data.templateParams;
      permissionExpInfoForm.expression = data.expression;
      permissionExpInfoForm.desc = data.desc;
    });
  } catch (err: any) {
    handleApiError(err, "获取权限表达式详情");
  }

  if (permissionExpInfoForm.templateId) {
    await handleGetParamConfigs(permissionExpInfoForm.templateId);
  }
};

/** 权限 */
const permissions = reactive([]);

/**
 * 获取权限表达式关联的权限表达式
 */
const handleGetPermissionExpPermissions = (id: string) => {
  getPermissionExpPermissions(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissions.length = 0;
        permissions.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限表达式关联的权限列表");
    });
};

/** 模板参数配置 */
const templateParamConfigs = reactive([]);
const handleGetParamConfigs = async (templateId: string) => {
  try {
    const result = await getPremissionExpTemplateParamConfigs(templateId);
    handleApiSuccess(result, (data: any) => {
      templateParamConfigs.length = 0;
      templateParamConfigs.push(...data);

      permissionExpInfoForm.templateParams = data.map((item: any) => {
        const param = permissionExpInfoForm.templateParams.find(
          (param: any) => param.code === item.code
        );
        return {
          code: item.code,
          value: param ? param.value : item.defaultValue,
        };
      });
    });
  } catch (err: any) {
    handleApiError(err, "获取限制条件模板参数配置");
  }
};

/**
 * 提交权限表达式表单
 */
const handlePermissionExpInfoFormSubmit = async () => {
  const validateResults = [];
  validateResults.push(permissionExpInfoFormRef.value.validate());

  const useTemplate = permissionExpInfoForm.templateId ? true : false;
  if (useTemplate) {
    validateResults.push(templateParamsRef.value.validate());
  }
  const result = await Promise.all(validateResults);
  const isValid = result.filter((item) => item).length === 0;
  if (!isValid) {
    return;
  }
  updatePermissionExp({
    ...permissionExpInfoForm,
    useTemplate,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetPermissionExpDetail(permissionExpId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新权限表达式");
    });
};

/**
 * 重置权限表达式表单
 */
const handleResetPermissionExpInfoForm = () => {
  permissionExpInfoFormRef.value.resetFields();
  if (permissionExpInfoForm.templateId) {
    templateParamsRef.value.reset();
  }
  handleGetPermissionExpDetail(permissionExpId.value);
};

/**
 * 删除授权条件
 *
 * @param authorizeId 授权ID
 */
const handleRemoveAuthorizeCondition = (authorizeId: string) => {
  Modal.warning({
    title: "确定取消限制吗？",
    content: "此操作将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeAuthorizeCondition({
        authorizeIds: [authorizeId],
        permissionExpIds: [permissionExpId.value],
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("取消限制成功");
            handleGetPermissionExpDetail(permissionExpId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除授权条件");
        });
    },
  });
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
      active_tab: "user_group_info",
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
      active_tab: "user_info",
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
      active_tab: "role_info",
    },
  });
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

/**
 * 跳转调试权限表达式
 */
const handleToDebugPermissionExp = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.permissionExp = {
    id: permissionExpId.value,
    name: permissionExpName.value,
    expression: permissionExpInfoForm.expression,
  };
  globalVariables.saveData();
  router.push({
    path: "/permission/expression/debug",
  });
};

/** 调试运行弹框 */
const debugDrawerVisible = ref(false);

export default defineComponent({
  components: {
    ParamInput,
  },
  setup() {
    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "condition_info";
      templateParamConfigs.length = 0;
      handleGetTemplateList();
      handleTabInit(activeTab.value, getQueryString("id"));
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      templateList,
      templateParamConfigs,
      permissionExpId,
      permissionExpName,
      templateParamsRef,
      permissionExpInfoFormRef,
      permissionExpInfoForm,
      permissionExpInfoFormRules,
      permissions,
      handeToPrincipalDetail,
      handleToResourceGroupDetail,
      handleToResourceDetail,
      handleToPermissionDetail,
      handleRemoveAuthorizeCondition,
      handlePermissionExpInfoFormSubmit,
      handleResetPermissionExpInfoForm,
      handleToDebugPermissionExp,
      debugDrawerVisible
    };
  },
});
