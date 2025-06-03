import {createIdentitySource} from "@/api/identitySource";
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

/**
 * 身份源提供商
 */
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
  return `${getOAuthIssuer()}/login/federation/callback/${
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
  createIdentitySourceFormRef.value.resetFields();
};

/**
 * 提交创建身份源表单
 */
const handleCreateIdentitySourceFormSubmit = (formData: any) => {
  const requestData = {
    ...formData,
    providerId: identitySourceProvider.id,
  };
  if (requestData.additionalParams) {
    requestData.additionalParams = JSON.parse(formData.additionalParams);
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
      identitySourceProvider.id = globalVariables.identitySourceProvider.id;
      identitySourceProvider.name = globalVariables.identitySourceProvider.name;
    });

    return {
      handleBack,
      identitySourceProvider,
      createIdentitySourceForm,
      createIdentitySourceFormRef,
      createIdentitySourceFormRules,
      callBackUrl,
      generateRandomIdentitySourceCode,
      handleResetCreateIdentitySourceForm,
      handleCreateIdentitySourceFormSubmit,
    };
  },
});
