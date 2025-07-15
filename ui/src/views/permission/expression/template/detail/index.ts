import {
    debugPermissionExp,
    getPermissionExpTemplateDetail,
    getPermissionExpTemplateExpList,
    updatePermissionExpTemplate,
} from "@/api/permission";
import router from "@/router";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";
import ParamSelect, {ParamConfig} from "../components/ParamSelect.vue";
import ParamInput from "../../components/ParamInput.vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("template_info");

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
const handleTabInit = (tabKey: string, id: string = templateId.value) => {
  switch (tabKey) {
    case "template_info":
      handleGetTemplateDetail(id);
      break;
    case "expression_list":
      if (!templateId) {
        handleGetTemplateDetail(id);
      }
      handleGetExpList(id);
      break;
  }
};

const templateName = ref(null);
const templateId = ref(null);

/**
 * 模板信息表单
 */
const templateInfoFormRef = ref();
const paramConfigRefs = ref([]);
const templateInfoForm = reactive({
  id: undefined,
  name: undefined,
  desc: undefined,
  expression: undefined,
  paramConfigs: [],
});
const templateInfoFormRules = {
  name: [{ required: true, message: "模板名称未填写" }],
  desc: [{ required: true, message: "模板描述未填写" }],
  expression: [{ required: true, message: "JEXL 表达式未填写" }],
};

/**
 * 获取模板详情
 */
const handleGetTemplateDetail = async (id: string = templateId.value) => {
  try {
    const result = await getPermissionExpTemplateDetail(id);
    handleApiSuccess(result, (data: any) => {
      templateName.value = data.name;
      templateId.value = data.id;

      templateInfoForm.id = data.id;
      templateInfoForm.name = data.name;
      templateInfoForm.desc = data.desc;
      templateInfoForm.expression = data.expression;
      templateInfoForm.paramConfigs = data.paramConfigs;

      paramConfigRefs.value = [];
    });
  } catch (err) {
    handleApiError(err, "获取权限表达式模板详情");
  }
};

/**
 * 模板参数类型
 */
const parmaTypes = [
  { value: "STRING", label: "字符串" },
  { value: "NUMBER", label: "数值" },
  { value: "BOOLEAN", label: "布尔值" },
  { value: "LIST", label: "列表" },
  { value: "CHOICE", label: "选择" },
];

/**
 * 添加参数配置对话框
 */
const addParamConfigModalVisible = ref(false);
const selectedParamType = ref("STRING");

/**
 * 打开添加参数配置对话框
 */
const handleParamConfigModalOpen = () => {
  addParamConfigModalVisible.value = true;
  selectedParamType.value = "STRING";
};

/**
 * 关闭添加参数配置对话框
 */
const handleParamConfigModalClose = () => {
  addParamConfigModalVisible.value = false;
  selectedParamType.value = "STRING";
};

/**
 * 添加参数配置
 */
const handleParamConfigModalConfirm = () => {
  const paramConfig: ParamConfig = {
    name: undefined,
    type: selectedParamType.value,
    required: false,
    defaultValue: undefined,
    options: undefined,
    code: undefined,
  };

  if (selectedParamType.value === "CHOICE") {
    paramConfig.options = [""];
  }

  templateInfoForm.paramConfigs.push(paramConfig);
};

/**
 * 移除参数配置
 */
const handleRemoveParamConfig = (index: number) => {
  Modal.confirm({
    title: `确定删除「模板参数 - ${index + 1}」吗？`,
    content: "",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      templateInfoForm.paramConfigs.splice(index, 1);
    },
  });
};

/**
 * 重置模板信息表单
 */
const handleResetTemplateInfoForm = () => {
  templateInfoFormRef.value.resetFields();
  paramConfigRefs.value.forEach((paramConfig) => paramConfig.reset());
  handleGetTemplateDetail();
};

/**
 * 提交模板信息表单
 */
const handleTemplateInfoFormSubmit = async () => {
  const validateResults = [];
  validateResults.push(templateInfoFormRef.value.validate());
  paramConfigRefs.value.forEach((paramConfig) =>
    validateResults.push(paramConfig.validate())
  );
  const result = await Promise.all(validateResults);
  const isValid = result.filter((item) => item).length === 0;
  if (!isValid) {
    return;
  }

  updatePermissionExpTemplate(templateInfoForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetTemplateDetail();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新权限表达式模板");
    });
};

/**
 * 关联的权限表达式列表
 */
const expList = reactive([]);
const handleGetExpList = (id: string = templateId.value) => {
  getPermissionExpTemplateExpList(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        expList.length = 0;
        expList.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取模板关联的权限表达式列表");
    });
};

/**
 * 跳转至权限表达式详情
 */
const handleToExpDetail = (exp: any) => {
  router.push({
    path: "/permission/expression/detail",
    query: {
      id: exp.id,
    },
  });
};

/** 调试运行弹框 */
const debugDrawerVisible = ref(false);
const debugParamConfigs = reactive([]);
const debugParamRef = ref();
const debugFormRef = ref();
const debugForm = reactive({
  templateId: undefined,
  useTemplate: true,
  templateParams: [],
  context: undefined,
});
const debugFormRules = {
  context: {
    validator: (value, cb) => {
      if (value) {
        try {
          const valueObj = JSON.parse(value);
          if (typeof valueObj !== "object") {
            cb("上下文 JSON 字符串必须是一个对象");
          }
        } catch (err: any) {
          cb("请检查 JSON 格式是否正确");
        }
      } else {
        cb();
      }
    },
  },
};

/**
 * 打开调试运行弹框
 */
const handleOpenDebugDrawer = () => {
  debugForm.templateId = templateId.value;
  debugForm.templateParams.length = 0;
  debugParamConfigs.length = 0;
  if (templateInfoForm.paramConfigs) {
    debugParamConfigs.push(...templateInfoForm.paramConfigs);
    const params = templateInfoForm.paramConfigs.map((item) => {
      return {
        code: item.code,
        value: item.defaultValue,
      };
    });
    debugForm.templateParams.push(...params);
  }
  debugDrawerVisible.value = true;
};

/**
 * 关闭调试运行弹框
 */
const handleCloseDebugDrawer = () => {
  debugFormRef.value.resetFields();
  debugParamRef.value.reset();
  debugDrawerVisible.value = false;
};

/** 调试运行结果 */
const debugResult = reactive({
  success: undefined,
  execResult: undefined,
});
const debugResultModalVisible = ref(false);

/**
 * 提交调试运行表单
 */
const debugFormSubmitLoading = ref(false);
const handleDebugFormSubmit = async () => {
  const validateResults = [];
  validateResults.push(debugFormRef.value.validate());
  validateResults.push(debugParamRef.value.validate());
  const result = await Promise.all(validateResults);
  const isValid = result.filter((item) => item).length === 0;
  if (!isValid) {
    return;
  }

  debugFormSubmitLoading.value = true;
  debugPermissionExp({
    templateId: debugForm.templateId,
    useTemplate: debugForm.useTemplate,
    templateParams: debugForm.templateParams,
    context: debugForm.context ? JSON.parse(debugForm.context) : {},
  })
    .then((result) => {
      handleApiSuccess(result, (data: any) => {
        debugResult.success = data.success;
        debugResult.execResult = data.executeRes;

        debugResultModalVisible.value = true;
      });
    })
    .catch((err) => {
      handleApiError(err, "调试运行限制条件模板");
    })
    .finally(() => {
      debugFormSubmitLoading.value = false;
    });
};

export default defineComponent({
  components: {
    ParamSelect,
    ParamInput,
  },
  setup() {
    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "template_info";
      handleTabInit(activeTab.value, getQueryString("id"));
    });

    return {
      handleBack,
      templateName,
      templateId,
      activeTab,
      handleTabChange,
      templateInfoFormRef,
      paramConfigRefs,
      templateInfoForm,
      templateInfoFormRules,
      parmaTypes,
      addParamConfigModalVisible,
      selectedParamType,
      handleParamConfigModalOpen,
      handleParamConfigModalClose,
      handleParamConfigModalConfirm,
      handleRemoveParamConfig,
      handleTemplateInfoFormSubmit,
      handleResetTemplateInfoForm,
      expList,
      handleToExpDetail,
      debugDrawerVisible,
      debugForm,
      debugFormRef,
      debugParamRef,
      debugFormRules,
      debugParamConfigs,
      handleOpenDebugDrawer,
      handleCloseDebugDrawer,
      debugFormSubmitLoading,
      handleDebugFormSubmit,
      debugResult,
      debugResultModalVisible,
    };
  },
});
