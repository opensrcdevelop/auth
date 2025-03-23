import {
  getMailMessageConfig,
  getMailServiceConfig,
  getMailTemplateDetail,
  getMailTemplateList,
  saveMailMessageConfig,
  saveMailServiceConfig,
  updateMailTemplate,
} from "@/api/setting";
import router from "@/router";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";

const activeTab = ref("mail_template");

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
    case "mail_template":
      handleGetMailTemplateList();
      break;
    case "mail_service":
      handleGetMailServiceConfig();
      handleGetMailMessageConfig();
      break;
  }
};

/**  邮件模版 */
const mailTemplateList = reactive([]);

const handleGetMailTemplateList = () => {
  getMailTemplateList()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        mailTemplateList.length = 0;
        mailTemplateList.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取邮件模版列表");
    });
};

/**
 * 邮件模版详情
 */
const editMailTemplateDrawerVisible = ref(false);
const mailTemplateDetailForm = reactive({
  id: undefined,
  name: undefined,
  content: undefined,
  parameters: undefined,
  subject: undefined,
  sender: undefined,
});
const mailTemplateDetailFormRef = ref(null);
const mailTemplateDetailFormRules = {
  subject: [{ required: true, message: "主题未填写" }],
  sender: [{ required: true, message: "发件人未填写" }],
  content: [{ required: true, message: "模版未填写" }],
};

/**
 * 前往编辑邮件模版
 */
const handleToEditMailTemplate = async (id: string) => {
  await handleGetMailTemplateDetail(id);
  editMailTemplateDrawerVisible.value = true;
};

/**
 * 获取邮件模版详情
 */
const handleGetMailTemplateDetail = async (id: string) => {
  getMailTemplateDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        mailTemplateDetailForm.id = data.id;
        mailTemplateDetailForm.name = data.name;
        mailTemplateDetailForm.content = data.content;
        mailTemplateDetailForm.parameters = data.parameters;
        mailTemplateDetailForm.subject = data.subject;
        mailTemplateDetailForm.sender = data.sender;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取邮件模版详情");
    });
};

/**
 * 重置邮件模版详情
 */
const handleResetMailTemplateDetailForm = () => {
  handleGetMailTemplateDetail(mailTemplateDetailForm.id);
  mailTemplateDetailFormRef.value.resetFields();
};

/**
 * 更新邮件模版
 */
const handleMailTemplateDetailFormSubmit = () => {
  updateMailTemplate(mailTemplateDetailForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        handleGetMailTemplateDetail(mailTemplateDetailForm.id);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新邮件模版");
    });
};

/** 邮件服务配置 */
const mailServiceConfigForm = reactive({
  host: undefined,
  port: undefined,
  username: undefined,
  password: undefined,
  sslEnable: false,
});
const mailServiceConfigFormRef = ref(null);
const mailServiceConfigFormRules = {
  host: [{ required: true, message: "主机未填写" }],
  port: [{ required: true, message: "端口未填写" }],
  username: [{ required: true, message: "用户名未填写" }],
  password: [{ required: true, message: "密码未填写" }],
  sslEnable: [{ required: true, message: "是否启用SSL未选择" }],
};

const handleResetMailServiceConfigForm = () => {
  mailServiceConfigFormRef.value.resetFields();
  handleGetMailServiceConfig();
};

/**
 * 获取邮件服务配置
 */
const handleGetMailServiceConfig = () => {
  getMailServiceConfig()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        mailServiceConfigForm.host = data.host;
        mailServiceConfigForm.port = data.port;
        mailServiceConfigForm.username = data.username;
        mailServiceConfigForm.password = data.password;
        mailServiceConfigForm.sslEnable = data.sslEnable || false;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取邮件服务配置");
    });
};

/**
 * 保存邮件服务配置
 */
const handleMailServiceConfigFormSubmit = () => {
  saveMailServiceConfig(mailServiceConfigForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetMailServiceConfig();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "保存邮件服务配置");
    });
};

/** 邮件消息配置 */
const mailMessageConfigForm = reactive({
  codeLive: undefined,
});
const mailMessageConfigFormRef = ref(null);
const mailMessageConfigFormRules = {
  codeLive: [{ required: true, message: "验证码有效期未填写" }],
};

const handleResetMailMessageConfigForm = () => {
  mailMessageConfigFormRef.value.resetFields();
  handleGetMailMessageConfig();
};

/**
 * 获取邮件消息配置
 */
const handleGetMailMessageConfig = () => {
  getMailMessageConfig()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        mailMessageConfigForm.codeLive = data.codeLive;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取邮件消息配置");
    });
};

/**
 * 保存邮件消息配置
 */
const handleMailMessageConfigFormSubmit = () => {
  saveMailMessageConfig(mailMessageConfigForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetMailMessageConfig();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "保存邮件消息配置");
    });
};

export default defineComponent({
  setup() {
    const tab = getQueryString("active_tab");
    const globalVariables = useGlobalVariablesStore();

    onMounted(() => {
      activeTab.value = tab || "mail_template";
      handleTabInit(activeTab.value);
    });

    return {
      activeTab,
      handleTabChange,
      globalVariables,
      mailTemplateList,
      handleToEditMailTemplate,
      editMailTemplateDrawerVisible,
      mailTemplateDetailForm,
      mailTemplateDetailFormRules,
      handleResetMailTemplateDetailForm,
      handleMailTemplateDetailFormSubmit,
      mailServiceConfigForm,
      mailServiceConfigFormRef,
      mailServiceConfigFormRules,
      handleResetMailServiceConfigForm,
      handleMailServiceConfigFormSubmit,
      mailMessageConfigForm,
      mailMessageConfigFormRef,
      mailMessageConfigFormRules,
      handleResetMailMessageConfigForm,
      handleMailMessageConfigFormSubmit,
    };
  },
});
