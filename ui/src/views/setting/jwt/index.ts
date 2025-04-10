import {
    getJwtSecretInfo,
    getJwtSecretRotationConfig,
    rotateJwtSecret,
    saveJwtSecretRotationConfig,
} from "@/api/setting";
import router from "@/router";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";

const activeTab = ref("jwt_secret");

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
  handleTabInit(tabKey);
};

const handleTabInit = (key: string) => {
  switch (key) {
    case "jwt_secret":
      handleGetSecretInfo();
      handleGetRotationConfig();
      break;
  }
};

/**
 * 密钥信息
 */
const secretInfo = reactive({
  kid: undefined,
  alg: undefined,
  createTime: undefined,
  expireTime: undefined,
});

const handleGetSecretInfo = () => {
  getJwtSecretInfo()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        secretInfo.kid = data.kid;
        secretInfo.alg = data.alg;
        secretInfo.createTime = data.createTime;
        secretInfo.expireTime = data.expireTime;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取 JWT 密钥信息");
    });
};

/**
 * 密钥轮换配置
 */
const rotationConfigForm = reactive({
  rotationPeriod: undefined,
  rotationPeriodUnit: undefined,
});
const rotationConfigFormRef = ref(null);
const rotationConfigFormRules = {
  rotationPeriod: [{ required: true, message: "轮换周期未填写" }],
  rotationPeriodUnit: [{ required: true, message: "轮换周期单位未选择" }],
};

const handleGetRotationConfig = () => {
  getJwtSecretRotationConfig()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        rotationConfigForm.rotationPeriod = data.rotationPeriod;
        rotationConfigForm.rotationPeriodUnit = data.rotationPeriodUnit;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取 JWT 密钥轮换配置");
    });
};

const handleRotationConfigFormSubmit = () => {
  saveJwtSecretRotationConfig(rotationConfigForm)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        Notification.success("保存成功");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "保存 JWT 密钥轮换配置");
    });
};

const handleResetRotationConfigForm = () => {
  rotationConfigFormRef.value.resetFields();
  handleGetRotationConfig();
};

const handleRotateSecret = () => {
  Modal.warning({
    title: `确定立即轮换密钥？`,
    content: "轮换后，所有使用旧密钥签发的 Token 将失效，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "warning",
    },
    onOk: () => {
      rotateJwtSecret()
        .then((result: any) => {
          Notification.success("密钥轮换成功");
          handleGetSecretInfo();
        })
        .catch((err: any) => {
          handleApiError(err, "密钥轮换");
        });
    },
  });
};

export default defineComponent({
  setup() {
    const tab = getQueryString("active_tab");

    onMounted(() => {
      activeTab.value = tab || "jwt_secret";
      handleTabInit(activeTab.value);
    });

    return {
      activeTab,
      handleTabChange,
      secretInfo,
      rotationConfigForm,
      rotationConfigFormRef,
      rotationConfigFormRules,
      handleRotationConfigFormSubmit,
      handleResetRotationConfigForm,
      handleRotateSecret
    };
  },
});
