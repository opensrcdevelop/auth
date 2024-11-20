import { defineComponent, h, nextTick, onMounted, reactive, ref } from "vue";
import router from "@/router";
import {
  deleteClient,
  getClientDetail,
  getOidcEndpointInfo,
  updateClientDetail,
  updateClientSecret,
} from "@/api/client";
import { useRoute } from "vue-router";
import { getOAuthIssuer, handleApiError, handleApiSuccess } from "@/util/tool";
import { Message, Modal, Notification } from "@arco-design/web-vue";
import {
  createOidcClaim,
  createOidcScope,
  deleteOidcClaim,
  deleteOidcScope,
  getOidcClaims,
  getOidcScopes,
  updateOidcClaim,
  updateOidcScope,
} from "@/api/oidc";
import { getUserAttrs } from "@/api/user";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("client_setting");

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

/** 基本信息 */
const clientBasicInfoForm = reactive({
  id: "",
  name: "",
  desc: "",
});

const clientBasicInfoFormRules = {
  name: [
    {
      required: true,
      message: "客户端名称未填写",
    },
  ],
};

const clientBasicInfoFormRef = ref();

const clientName = ref("");

/** 端点信息 */
const clientEndpointInfo = reactive({
  id: "",
  issuer: "",
  openidConfiguration: `${getOAuthIssuer()}/.well-known/openid-configuration`,
  jwks: "",
  authorize: "",
  token: "",
  userinfo: "",
});

/** 认证信息 */
const clientAuthInfoForm = reactive({
  id: "",
  redirectUri: "",
});

const clientAuthInfoFormRules = {
  redirectUri: [
    {
      required: true,
      message: "登录回调 URL 未填写",
    },
  ],
};

const clientAuthInfoFormRef = ref();

/**
 * 获取客户端详情
 */
const handleGetClientDetail = (id: string) => {
  getClientDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        clientBasicInfoForm.id = data.id;
        clientBasicInfoForm.name = data.name;
        clientBasicInfoForm.desc = data.desc;
        clientName.value = data.name;

        clientEndpointInfo.id = data.id;

        clientAuthInfoForm.id = data.id;
        clientAuthInfoForm.redirectUri = data.redirectUri;

        clientAuthorizeInfoForm.id = data.id;
        clientAuthorizeInfoForm.grantTypes = data.grantTypes;
        clientAuthorizeInfoForm.scopes = data.scopes;
        clientAuthorizeInfoForm.authenticationMethods =
          data.authenticationMethods;
        clientAuthorizeInfoForm.authorizationCodeTimeToLive =
          data.authorizationCodeTimeToLive;
        clientAuthorizeInfoForm.refreshTokenTimeToLive =
          data.refreshTokenTimeToLive;
        clientAuthorizeInfoForm.accessTokenTimeToLive =
          data.accessTokenTimeToLive;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取客户端详情");
    });
};

/**
 * 获取 Oidc 端点信息
 */
const handleGetOidcEndpointInfo = () => {
  getOidcEndpointInfo()
    .then((result: any) => {
      clientEndpointInfo.issuer = result.issuer;
      clientEndpointInfo.jwks = result.jwks_uri;
      clientEndpointInfo.authorize = result.authorization_endpoint;
      clientEndpointInfo.token = result.token_endpoint;
      clientEndpointInfo.userinfo = result.userinfo_endpoint;
    })
    .catch((err: any) => {
      handleApiError(err, "获取 Oidc 端点信息");
    });
};

/**
 * 更新客户端基本信息
 *
 * @param formData 客户端基本信息
 */
const handleClientBasicInfoFormSubmit = (formData) => {
  updateClientDetail(formData)
    .then((resutlt: any) => {
      handleApiSuccess(resutlt, () => {
        Notification.success("保存成功");
        handleGetClientDetail(formData.id);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新客户端基本信息");
    });
};

/**
 * 重置客户端基本信息表单
 */
const handleResetClientBasicInfoForm = () => {
  clientBasicInfoFormRef.value.resetFields();
  handleGetClientDetail(clientBasicInfoForm.id);
};

/**
 * 更新客户端认证配置
 *
 * @param formData 客户端认证配置
 */
const handleClientAuthInfoFormSubmit = (formData) => {
  updateClientDetail(formData)
    .then((resutlt: any) => {
      handleApiSuccess(resutlt, () => {
        Notification.success("保存成功");
        handleGetClientDetail(formData.id);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新客户端认证配置");
    });
};

/**
 * 重置客户端认证配置表单
 */
const handleResetClientAuthInfoForm = () => {
  clientAuthInfoFormRef.value.resetFields();
  handleGetClientDetail(clientAuthInfoForm.id);
};

const updateClientSecretSuccessModalVisible = ref(false);
const newClientSecret = ref("");

/**
 * 刷新客户端密钥
 *
 * @param id 客户端 ID
 */
const handleUpdateClientSecretSubmit = (id: string) => {
  updateClientSecret(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        newClientSecret.value = data.secret;
        updateClientSecretSuccessModalVisible.value = true;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "刷新客户端密钥");
    });
};

const clientAuthorizeInfoForm = reactive({
  id: "",
  grantTypes: [],
  authenticationMethods: [],
  scopes: [],
  authorizationCodeTimeToLive: 0,
  accessTokenTimeToLive: 0,
  refreshTokenTimeToLive: 0,
});

const clientAuthorizeInfoFormRules = {
  grantTypes: [
    {
      required: true,
      message: "至少选择一项",
    },
  ],
  authenticationMethods: [
    {
      required: true,
      message: "至少选择一项",
    },
  ],
  authorizationCodeTimeToLive: [
    {
      required: true,
      message: "授权码过期时间未填写",
    },
  ],
  accessTokenTimeToLive: [
    {
      required: true,
      message: "访问令牌过期时间未填写",
    },
  ],
  refreshTokenTimeToLive: [
    {
      required: true,
      message: "刷新令牌过期时间未填写",
    },
  ],
};

const authorizationCodeTimeToLiveUnit = ref(1);
const accessTokenTimeToLiveUnit = ref(1);
const refreshTokenTimeToLiveUnit = ref(1);

const clientAuthorizeInfoFormRef = ref();

/**
 * 更新客户端授权配置
 *
 * @param formData 客户端授权配置表单
 */
const handleClientAuthorizeInfoFormSubmit = (formData) => {
  formData.authorizationCodeTimeToLive =
    formData.authorizationCodeTimeToLive *
    authorizationCodeTimeToLiveUnit.value;
  formData.accessTokenTimeToLive =
    formData.accessTokenTimeToLive * accessTokenTimeToLiveUnit.value;
  formData.refreshTokenTimeToLive =
    formData.refreshTokenTimeToLive * refreshTokenTimeToLiveUnit.value;

  updateClientDetail(formData)
    .then((resutlt: any) => {
      handleApiSuccess(resutlt, () => {
        Notification.success("保存成功");
        authorizationCodeTimeToLiveUnit.value = 1;
        accessTokenTimeToLiveUnit.value = 1;
        refreshTokenTimeToLiveUnit.value = 1;
        handleGetClientDetail(formData.id);
        handleGetOidcScopes();
        handleGetOidcClaims();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新客户端授权配置");
    });
};

/**
 * 重置客户端授权配置
 */
const handleResetClientAuthorizeInfoForm = () => {
  clientAuthorizeInfoFormRef.value.resetFields();
  handleGetClientDetail(clientAuthInfoForm.id);
  handleGetOidcScopes();
};

const oidcScopes = reactive([]);

/**
 * 获取 OIDC Scope
 */
const handleGetOidcScopes = () => {
  getOidcScopes()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        oidcScopes.length = 0;
        oidcScopes.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取 OIDC Scope");
    });
};

const oidcClaims = reactive([]);

/**
 * 获取 OIDC Claim
 */
const handleGetOidcClaims = () => {
  getOidcClaims()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        oidcClaims.length = 0;
        oidcClaims.push(...data);

        claimMappings.length = 0;
        claimMappings.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取 OIDC Claim");
    });
};

/**
 * 保存 OIDC Scope
 *
 * @param scope OIDC scope
 */
const handleSaveOidcScopeSubmit = (scope: any) => {
  if (scope.name.trim() === "") {
    Message.warning("Scope 名称不能为空");
    return;
  }

  // 更新 OIDC scope
  if (scope.id) {
    updateOidcScope(scope)
      .then((result: any) => {
        handleApiSuccess(result, () => {
          Notification.success("保存成功");
          handleGetOidcScopes();
        });
      })
      .catch((err: any) => {
        handleApiError(err, "更新 OIDC Scope");
      });
  } else {
    // 创建 OIDC scope
    createOidcScope(scope)
      .then((result) => {
        handleApiSuccess(result, () => {
          Notification.success("创建成功");
          handleGetOidcScopes();
        });
      })
      .catch((err: any) => {
        handleApiError(err, "创建 OIDC Scope");
      });
  }
};

/**
 * 添加 OIDC scope
 */
const handleCreateOidcScope = () => {
  let scope = {
    name: "",
    claims: [],
  };
  oidcScopes.push(scope);
};

/**
 * 移除 OIDC Scope
 *
 * @param scope OIDC Scope
 */
const handleRemoveOidcScope = (scope: any) => {
  oidcScopes.splice(oidcScopes.indexOf(scope), 1);
};

/**
 * 删除 OIDC Scope
 *
 * @param scope OIDC Scope
 */
const handleDeleteOidcScopeSubmit = (scope: any) => {
  deleteOidcScope(scope.id)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("删除成功");
        handleGetOidcScopes();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "删除 OIDC Scope");
    });
};

/** 用户属性 */
const userAttrs = reactive([]);
/** claim 字段映射 */
const claimMappings = reactive([]);

/**
 * 获取所有用户属性
 */
const handleGetUserAttrs = () => {
  getUserAttrs({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userAttrs.length = 0;
        userAttrs.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户属性");
    });
};

/**
 * 添加 OIDC claim
 */
const handleCreateOidcClaim = () => {
  let claim = {
    name: "",
    userAttrId: "",
  };
  claimMappings.push(claim);
};

/**
 * 移除 OIDC Claim
 *
 * @param scope OIDC Scope
 */
const handleRemoveOidcClaim = (claim: any) => {
  claimMappings.splice(claimMappings.indexOf(claim), 1);
};

/**
 * 保存 OIDC Claim
 *
 * @param claim OIDC Claim
 */
const handleSaveOidcClaimSubmit = (claim: any) => {
  if (claim.name.trim() === "") {
    Message.warning("Claim 名称不能为空");
    return;
  }

  if (claim.userAttrId.trim() === "") {
    Message.warning("请选择一个用户字段");
    return;
  }

  // 更新 OIDC Claim
  if (claim.id) {
    updateOidcClaim(claim)
      .then((result: any) => {
        handleApiSuccess(result, () => {
          Notification.success("保存成功");
          handleGetOidcClaims();
        });
      })
      .catch((err: any) => {
        handleApiError(err, "更新 OIDC Claim");
      });
  } else {
    // 创建 OIDC Claim
    createOidcClaim(claim)
      .then((result: any) => {
        handleApiSuccess(result, () => {
          Notification.success("创建成功");
          handleGetOidcClaims();
        });
      })
      .catch((err: any) => {
        handleApiError(err, "创建 OIDC Claim");
      });
  }
};

/**
 * 删除 OIDC Claim
 *
 * @param scope OIDC Claim
 */
const handleDeleteOidcClaimSubmit = (claim: any) => {
  deleteOidcClaim(claim.id)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("删除成功");
        handleGetOidcClaims();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "删除 OIDC Claim");
    });
};

/**
 * 删除客户端
 *
 * @param client 客户端
 */
const handleDeleteClientSubmit = (name: string, id: string) => {
  Modal.warning({
    title: `确定删除客户端「${name}」吗？`,
    content: "删除后将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteClient(id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleBack();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除客户端");
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
      const clientId = route.query.id as string;
      handleGetClientDetail(clientId);
      handleGetOidcEndpointInfo();
      handleGetOidcScopes();
      handleGetOidcClaims();
      handleGetUserAttrs();
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      clientBasicInfoForm,
      clientBasicInfoFormRules,
      handleClientBasicInfoFormSubmit,
      clientBasicInfoFormRef,
      clientEndpointInfo,
      clientAuthInfoForm,
      clientAuthInfoFormRef,
      clientName,
      handleResetClientBasicInfoForm,
      clientAuthInfoFormRules,
      handleClientAuthInfoFormSubmit,
      handleResetClientAuthInfoForm,
      handleUpdateClientSecretSubmit,
      updateClientSecretSuccessModalVisible,
      newClientSecret,
      clientAuthorizeInfoForm,
      clientAuthorizeInfoFormRules,
      handleClientAuthorizeInfoFormSubmit,
      clientAuthorizeInfoFormRef,
      handleResetClientAuthorizeInfoForm,
      authorizationCodeTimeToLiveUnit,
      accessTokenTimeToLiveUnit,
      refreshTokenTimeToLiveUnit,
      oidcScopes,
      oidcClaims,
      handleSaveOidcScopeSubmit,
      handleCreateOidcScope,
      handleRemoveOidcScope,
      handleDeleteOidcScopeSubmit,
      userAttrs,
      claimMappings,
      handleCreateOidcClaim,
      handleRemoveOidcClaim,
      handleSaveOidcClaimSubmit,
      handleDeleteOidcClaimSubmit,
      handleDeleteClientSubmit,
    };
  },
});
