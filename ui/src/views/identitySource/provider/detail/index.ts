import {
    deleteIdentitySourceProvider,
    getIdentitySourceProviderDetail,
    getIdentitySourceRegistrations,
    updateIdentitySource,
    updateIdentitySourceProvider,
} from "@/api/identitySource";
import router from "@/router";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";
import {useRoute} from "vue-router";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("provider_info");

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

const handleTabInit = (tabKey: string) => {
  switch (tabKey) {
    case "registration_list":
      handleGetProviderDetail();
      handleGetRegistrations();
      break;
    case "provider_info":
      handleGetProviderDetail();
  }
};

const providerId = ref("");
const providerName = ref("");

/**
 * 身份源提供商基本信息表单
 */
const providerBasicInfoFormRef = ref(null);
const providerBasicInfoForm = reactive({
  name: undefined,
  code: undefined,
  desc: undefined,
  logo: undefined,
  scopes: undefined,
});
const providerBasicInfoFormRules = {
  name: [{ required: true, message: "身份源提供商名称未填写" }],
  logo: [{ required: true, message: "身份源提供商 Logo 未填写" }],
  code: [
    { required: true, message: "身份源提供商标识未填写" },
    {
      validator: (value, cb) => {
        if (value && !/^[A-Za-z0-9]+$/.test(value)) {
          cb("只允许包含英文字母、数字");
        } else {
          cb();
        }
      },
    },
  ],
};

/**
 * 重置身份源提供商基本信息表单
 */
const handleResetProviderBasicInfoForm = () => {
  providerBasicInfoFormRef.value.resetFields();
  handleGetProviderDetail();
};

/**
 * 提交身份源提供商基本信息表单
 */
const handleProviderBasicInfoFormSubmit = (formData: any) => {
  updateIdentitySourceProvider({
    ...formData,
    id: providerId.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetProviderDetail();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新身份源提供商");
    });
};

/**
 * 身份源提供商端点信息表单
 */
const providerEndpointInfoFormRef = ref(null);
const providerEndpointInfoForm = reactive({
  authorizationUri: undefined,
  enableCustomAuthzReq: undefined,
  authzReqCfg: undefined,
  tokenUri: undefined,
  enableCustomTokenReq: undefined,
  tokenReqCfg: undefined,
  userInfoUris: undefined,
  enableCustomUserInfoReq: undefined,
  userInfoReqCfg: undefined,
  jwkSetUri: undefined,
  scopes: undefined,
});
const providerEndpointInfoFormRules = {
  authorizationUri: [{ required: true, message: "授权地址未填写" }],
  tokenUri: [{ required: true, message: "Token 地址未填写" }],
  userInfoUris: [{ required: true, message: "用户信息地址未填写" }],
  authzReqCfg: [
    {
      validator: (value, cb) => {
        if (providerEndpointInfoForm.enableCustomAuthzReq && !value) {
          cb("授权请求配置未填写");
        } else {
          cb();
        }
      },
    },
    {
      validator: (value, cb) => {
        try {
          JSON.parse(value);
          cb();
        } catch (err) {
          cb("授权请求配置 JSON 格式错误");
        }
      },
    },
  ],
  tokenReqCfg: [
    {
      validator: (value, cb) => {
        if (providerEndpointInfoForm.enableCustomTokenReq && !value) {
          cb("Token 请求配置未填写");
        } else {
          cb();
        }
      },
    },
    {
      validator: (value, cb) => {
        try {
          JSON.parse(value);
          cb();
        } catch (err) {
          cb("Token 请求配置 JSON 格式错误");
        }
      },
    },
  ],
  userInfoReqCfg: [
    {
      validator: (value, cb) => {
        if (providerEndpointInfoForm.enableCustomUserInfoReq && !value) {
          cb("用户信息请求配置未填写");
        } else {
          cb();
        }
      },
    },
    {
      validator: (value, cb) => {
        try {
          JSON.parse(value);
          cb();
        } catch (err) {
          cb("用户信息请求配置 JSON 格式错误");
        }
      },
    },
  ],
};

/**
 * 重置身份源提供商端点信息表单
 */
const handleResetProviderEndpointInfoForm = () => {
  providerEndpointInfoFormRef.value.resetFields();
  handleGetProviderDetail();
};

/**
 * 提交身份源提供商端点信息表单
 */
const handleProviderEndpointInfoFormSubmit = (formData: any) => {
  const requestData = {
    ...formData,
    id: providerId.value,
  };

  if (requestData.enableCustomAuthzReq) {
    requestData.authzReqCfg = JSON.parse(requestData.authzReqCfg);
  } else {
    requestData.authzReqCfg = undefined;
  }

  if (requestData.enableCustomTokenReq) {
    requestData.tokenReqCfg = JSON.parse(requestData.tokenReqCfg);
  } else {
    requestData.tokenReqCfg = undefined;
  }

  if (requestData.enableCustomUserInfoReq) {
    requestData.userInfoReqCfg = JSON.parse(requestData.userInfoReqCfg);
  } else {
    requestData.userInfoReqCfg = undefined;
  }

  updateIdentitySourceProvider(requestData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetProviderDetail();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新身份源提供商");
    });
};

/**
 * 删除用户信息端点
 */
const handleRemoveUserInfoUri = (index: number) => {
  providerEndpointInfoForm.userInfoUris.splice(index, 1);
};

/**
 * 添加用户信息端点
 */
const handleAddUserInfoUri = () => {
  if (providerEndpointInfoForm.userInfoUris === undefined) {
    providerEndpointInfoForm.userInfoUris = [];
  }
  providerEndpointInfoForm.userInfoUris.push("");
};

/**
 * 身份源提供商认证信息表单
 */
const providerAuthInfoFormRef = ref(null);
const providerAuthInfoForm = reactive({
  userInfoAuthenticationMethod: undefined,
  scopes: undefined,
  usernameAttribute: undefined,
  uniqueIdAttribute: undefined,
  userMatchAttribute: undefined,
});
const providerAuthInfoFormRules = {
  userInfoAuthenticationMethod: [
    { required: true, message: "用户信息认证方式未选择" },
  ],
  usernameAttribute: [{ required: true, message: "用户名属性未填写" }],
  uniqueIdAttribute: [{ required: true, message: "唯一标识属性未填写" }],
  userMatchAttribute: [{ required: true, message: "用户匹配属性未填写" }],
};

/**
 * 删除 scope
 */
const handleRemoveScope = (index: number) => {
  providerAuthInfoForm.scopes.splice(index, 1);
};

/**
 * 添加 scope
 */
const handleAddScope = () => {
  if (providerAuthInfoForm.scopes === undefined) {
    providerAuthInfoForm.scopes = [];
  }
  providerAuthInfoForm.scopes.push("");
};

/**
 * 重置身份源提供商认证信息表单
 */
const handleResetProviderAuthInfoForm = () => {
  providerAuthInfoFormRef.value.resetFields();
  handleGetProviderDetail();
};

/**
 * 提交身份源提供商认证信息表单
 */
const handleProviderAuthInfoFormSubmit = (formData: any) => {
  updateIdentitySourceProvider({
    ...formData,
    id: providerId.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetProviderDetail();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新身份源提供商");
    });
};

/**
 * 获取身份源提供商详情
 */
const handleGetProviderDetail = () => {
  getIdentitySourceProviderDetail(providerId.value)
    .then((reult: any) => {
      handleApiSuccess(reult, (data: any) => {
        providerName.value = data.name;
        providerId.value = data.id;

        providerBasicInfoForm.name = data.name;
        providerBasicInfoForm.code = data.code;
        providerBasicInfoForm.desc = data.desc;
        providerBasicInfoForm.logo = data.logo;
        providerBasicInfoForm.scopes = data.scopes;

        providerEndpointInfoForm.authorizationUri = data.authorizationUri;
        providerEndpointInfoForm.enableCustomAuthzReq =
          data.enableCustomAuthzReq;
        providerEndpointInfoForm.authzReqCfg = data.authzReqCfg;
        providerEndpointInfoForm.tokenUri = data.tokenUri;
        providerEndpointInfoForm.enableCustomTokenReq =
          data.enableCustomTokenReq;
        providerEndpointInfoForm.tokenReqCfg = data.tokenReqCfg;
        providerEndpointInfoForm.userInfoUris = data.userInfoUris;
        providerEndpointInfoForm.enableCustomUserInfoReq = data.enableCustomUserInfoReq;
        providerEndpointInfoForm.userInfoReqCfg = data.userInfoReqCfg;
        providerEndpointInfoForm.scopes = data.scopes;

        providerAuthInfoForm.userInfoAuthenticationMethod =
          data.userInfoAuthenticationMethod;
        providerAuthInfoForm.scopes = data.scopes;
        providerAuthInfoForm.usernameAttribute = data.usernameAttribute;
        providerAuthInfoForm.uniqueIdAttribute = data.uniqueIdAttribute;
        providerAuthInfoForm.userMatchAttribute = data.userMatchAttribute;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取身份源提供商详情");
    });
};

/**
 * 关联身份源列表
 */
const registrationList = reactive([]);

/**
 * 获取关联身份源列表
 */
const handleGetRegistrations = () => {
  getIdentitySourceRegistrations(providerId.value)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        registrationList.length = 0;
        registrationList.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取关联身份源列表");
    });
};

/**
 * 更新身份源状态
 */
const handleUpdateRegistrationState = (registration: any) => {
  updateIdentitySource({
    id: registration.id,
    enabled: registration.enabled,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("更新身份源状态成功");
        handleGetRegistrations();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新身份源状态");
    });
};

/**
 * 跳转创建身份源
 */
const handleToCreateRegistration = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.identitySourceProvider.id = providerId.value;
  globalVariables.identitySourceProvider.name = providerName.value;
  globalVariables.saveData();
  router.push({
    path: "/identitySource/create",
  });
};

/**
 * 跳转身份源详情
 */
const handleToRegistrationDetail = (registration: any) => {
  router.push({
    path: "/identitySource/detail",
    query: {
      id: registration.id,
      active_tab: "registration_info",
    },
  });
};

/**
 * 删除身份源提供商
 */
const handleDeleteProvider = () => {
  Modal.warning({
    title: `确定删除身份源提供商「${providerName.value}」吗？`,
    content: "此操作将删除该身份源提供商及关联的身份源和用户的绑定关系，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteIdentitySourceProvider(providerId.value)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleBack();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除身份源提供商");
        });
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const route = useRoute();
      const id = route.query.id as string;
      const tabName = getQueryString("active_tab") || "provider_info";
      activeTab.value = tabName;
      providerId.value = id;
      handleTabInit(tabName);
    });

    return {
      handleBack,
      providerName,
      providerId,
      activeTab,
      handleTabChange,
      providerBasicInfoForm,
      providerBasicInfoFormRef,
      providerBasicInfoFormRules,
      handleResetProviderBasicInfoForm,
      handleProviderBasicInfoFormSubmit,
      providerEndpointInfoForm,
      providerEndpointInfoFormRef,
      providerEndpointInfoFormRules,
      handleResetProviderEndpointInfoForm,
      handleProviderEndpointInfoFormSubmit,
      handleRemoveUserInfoUri,
      handleAddUserInfoUri,
      providerAuthInfoForm,
      providerAuthInfoFormRef,
      providerAuthInfoFormRules,
      handleResetProviderAuthInfoForm,
      handleProviderAuthInfoFormSubmit,
      handleRemoveScope,
      handleAddScope,
      registrationList,
      handleUpdateRegistrationState,
      handleToCreateRegistration,
      handleToRegistrationDetail,
      handleDeleteProvider
    };
  },
});
