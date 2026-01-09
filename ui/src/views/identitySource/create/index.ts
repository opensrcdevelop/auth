import {createIdentitySource, getIdentitySourceProviderList,} from "@/api/identitySource";
import router from "@/router";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import {generateRandomString, getOAuthIssuer, handleApiError, handleApiSuccess,} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {computed, defineComponent, onMounted, reactive, ref} from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 身份源提供商列表 */
const providerList = reactive([]);

/**
 * 获取身份源提供商列表
 */
const handleGetProviderList = () => {
  getIdentitySourceProviderList({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        providerList.length = 0;
        providerList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取身份源提供商列表");
    });
};

/**
 * 身份源提供商
 */
const identitySourceProviderRef = ref(null);
const identitySourceProvider = reactive({
  id: undefined,
  name: undefined,
});

/**
 * 创建身份源表单
 */
const createIdentitySourceForm = reactive({
  providerId: undefined,
  name: undefined,
  code: undefined,
  clientId: undefined,
  clientSecret: undefined,
  clientAuthenticationMethod: "client_secret_basic",
  authorizationGrantType: "authorization_code",
  additionalParams: undefined,
});
const createIdentitySourceFormRef = ref();
const createIdentitySourceFormRules = {
  name: [{ required: true, message: "身份源显示名称未填写" }],
  code: [{ required: true, message: "身份源标识未填写" }],
  clientId: [{ required: true, message: "Client ID 未填写" }],
  clientSecret: [{ required: true, message: "Client Secret 未填写" }],
  clientAuthenticationMethod: [
    { required: true, message: "客户端认证方式未选择" },
  ],
  authorizationGrantType: [{ required: true, message: "授权类型未选择" }],
  additionalParams: [
    {
      validator: (value, cb) => {
        try {
          if (value) {
            JSON.parse(value);
            cb();
          }
        } catch (e) {
          cb("额外参数 JSON 格式错误");
        }
      },
    },
  ],
};

// 回调地址
const callBackUrl = computed(() => {
  return `${getOAuthIssuer()}${import.meta.env.VITE_API_BASE_URI}/login/federation/callback/${
    createIdentitySourceForm.code ? createIdentitySourceForm.code : "身份源标识"
  }`;
});

/**
 * 生成随机身份源标识
 */
const generateRandomIdentitySourceCode = () => {
  createIdentitySourceForm.code = generateRandomString(10);
};

/**
 * 重置创建身份源表单
 */
const handleResetCreateIdentitySourceForm = () => {
  if (identitySourceProviderRef.value) {
    identitySourceProviderRef.value.resetFields();
    handleGetProviderList();
  }
  createIdentitySourceFormRef.value.resetFields();
};

/**
 * 提交创建身份源表单
 */
const handleCreateIdentitySourceFormSubmit = async () => {
  let hasError = false;
  if (identitySourceProviderRef.value) {
    await identitySourceProviderRef.value.validate((errors) => {
      if (errors) {
        hasError = true;
      }
    });
  }

  await createIdentitySourceFormRef.value.validate((errors) => {
    if (errors) {
      hasError = true;
    }
  });

  if (hasError) {
    return;
  }

  const requestData = {
    ...createIdentitySourceForm,
    providerId: identitySourceProvider.id,
  };
  if (requestData.additionalParams) {
    requestData.additionalParams = JSON.parse(
      createIdentitySourceForm.additionalParams
    );
  } else {
    requestData.additionalParams = undefined;
  }

  createIdentitySource(requestData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateIdentitySourceForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建身份源");
    });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const globalVariables = useGlobalVariablesStore().getData();
      if (
        globalVariables.identitySourceProvider.id &&
        globalVariables.identitySourceProvider.name
      ) {
        identitySourceProvider.id = globalVariables.identitySourceProvider.id;
        identitySourceProvider.name =
          globalVariables.identitySourceProvider.name;
      } else {
        identitySourceProvider.id = undefined;
        identitySourceProvider.name = undefined;
        handleGetProviderList();
      }
    });

    return {
      handleBack,
      identitySourceProviderRef,
      identitySourceProvider,
      createIdentitySourceForm,
      createIdentitySourceFormRef,
      createIdentitySourceFormRules,
      callBackUrl,
      generateRandomIdentitySourceCode,
      handleResetCreateIdentitySourceForm,
      handleCreateIdentitySourceFormSubmit,
      providerList,
    };
  },
});
