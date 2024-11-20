import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { createClient } from "@/api/client";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { getOidcScopes } from "@/api/oidc";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const createClientForm = reactive({
  name: "",
  desc: "",
  redirectUri: "",
  grantTypes: [],
  scopes: [],
  authenticationMethods: [],
  authorizationCodeTimeToLive: 5,
  accessTokenTimeToLive: 2,
  refreshTokenTimeToLive: 7,
});

const oidcScopes = reactive([]);

const authorizationCodeTimeToLiveUnit = ref(1);
const accessTokenTimeToLiveUnit = ref(60);
const refreshTokenTimeToLiveUnit = ref(1440);

const createClientFormRef = ref();
const createClientFormRules = {
  name: [
    {
      required: true,
      message: "客户端名称未填写",
    },
  ],
  redirectUri: [
    {
      required: true,
      message: "登录回调 URL未填写",
    },
  ],
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

const createClientSuccessModalVisible = ref(false);
const clientSecret = ref("");

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
      handleApiError(err, "获取 OIDC Scope 失败");
    });
};

/**
 * 创建客户端
 *
 * @param formData 创建客户端表单
 */
const handleCreateClientFormSubmit = (formData) => {
  createClient({
    name: formData.name,
    desc: formData.desc,
    redirectUri: formData.redirectUri,
    grantTypes: formData.grantTypes,
    authenticationMethods: formData.authenticationMethods,
    scopes: formData.scopes,
    authorizationCodeTimeToLive:
      formData.authorizationCodeTimeToLive *
      authorizationCodeTimeToLiveUnit.value,
    accessTokenTimeToLive:
      formData.accessTokenTimeToLive * accessTokenTimeToLiveUnit.value,
    refreshTokenTimeToLive:
      formData.refreshTokenTimeToLive * refreshTokenTimeToLiveUnit.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        clientSecret.value = result.data.secret;
        createClientSuccessModalVisible.value = true;
        handleResetCreateClientForm();
        authorizationCodeTimeToLiveUnit.value = 1;
        accessTokenTimeToLiveUnit.value = 60;
        refreshTokenTimeToLiveUnit.value = 1440;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建客户端");
    });
};

/**
 * 重置创建客户端表单
 */
const handleResetCreateClientForm = () => {
  createClientFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetOidcScopes();
    });

    return {
      handleBack,
      createClientForm,
      createClientFormRules,
      createClientFormRef,
      authorizationCodeTimeToLiveUnit,
      accessTokenTimeToLiveUnit,
      refreshTokenTimeToLiveUnit,
      handleCreateClientFormSubmit,
      handleResetCreateClientForm,
      oidcScopes,
      createClientSuccessModalVisible,
      clientSecret,
    };
  },
});
