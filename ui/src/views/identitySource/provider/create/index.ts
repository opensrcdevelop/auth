import {createIdentitySourceProvider} from "@/api/identitySource";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {defineComponent, reactive, ref} from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/**
 * 创建身份源提供商表单
 */
const createProviderForm = reactive({
  name: undefined,
  code: undefined,
  desc: undefined,
  logo: undefined,
  authorizationUri: undefined,
  enableCustomAuthzReq: undefined,
  authzReqCfg: undefined,
  tokenUri: undefined,
  enableCustomTokenReq: undefined,
  tokenReqCfg: undefined,
  userInfoUris: [undefined],
  jwkSetUri: undefined,
  enableCustomUserInfoReq: undefined,
  userInfoReqCfg: undefined,
  userInfoAuthenticationMethod: "header",
  scopes: [""],
  usernameAttribute: undefined,
  userMatchAttribute: undefined,
});
const createProviderFormRef = ref(null);
const createProviderFormRules = {
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
  authorizationUri: [{ required: true, message: "授权地址未填写" }],
  tokenUri: [{ required: true, message: "Token 地址未填写" }],
  userInfoUris: [{ required: true, message: "用户信息地址未填写" }],
  userInfoAuthenticationMethod: [
    { required: true, message: "用户信息认证方式未填写" },
  ],
  usernameAttribute: [{ required: true, message: "用户名属性未填写" }],
  userMatchAttribute: [{ required: true, message: "用户匹配属性未填写" }],
  authzReqCfg: [
    {
      validator: (value, cb) => {
        if (createProviderForm.enableCustomAuthzReq && !value) {
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
        if (createProviderForm.enableCustomTokenReq && !value) {
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
        if (createProviderForm.enableCustomUserInfoReq && !value) {
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
 * 删除用户信息端点
 */
const handleRemoveUserInfoUri = (index: number) => {
  createProviderForm.userInfoUris.splice(index, 1);
};

/**
 * 添加用户信息端点
 */
const handleAddUserInfoUri = () => {
  if (createProviderForm.userInfoUris === undefined) {
    createProviderForm.userInfoUris = [];
  }
  createProviderForm.userInfoUris.push("");
};

/**
 * 删除 scope
 */
const handleRemoveScope = (index: number) => {
  createProviderForm.scopes.splice(index, 1);
};

/**
 * 添加 scope
 */
const handleAddScope = () => {
  if (createProviderForm.scopes === undefined) {
    createProviderForm.scopes = [];
  }
  createProviderForm.scopes.push("");
};

/**
 * 重置创建身份源提供商表单
 */
const handleResetCreateProviderForm = () => {
  createProviderFormRef.value.resetFields();
};

/**
 * 提交创建身份源提供商表单
 */
const handleCreateProviderFormSubmit = (formData: any) => {
  const requestData = {
    ...formData,
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

  createIdentitySourceProvider(requestData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateProviderForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建身份源提供商");
    });
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createProviderForm,
      createProviderFormRef,
      createProviderFormRules,
      handleRemoveUserInfoUri,
      handleAddUserInfoUri,
      handleRemoveScope,
      handleAddScope,
      handleResetCreateProviderForm,
      handleCreateProviderFormSubmit,
    };
  },
});
