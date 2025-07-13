import router from "@/router";
import {defineComponent, reactive, ref} from "vue";
import ParamSelect, {ParamConfig} from "../components/ParamSelect.vue";
import {Modal, Notification} from "@arco-design/web-vue";
import {createPermissionExpTemplate} from "@/api/permission";
import {handleApiError, handleApiSuccess} from "@/util/tool";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/**
 * 模板信息表单
 */
const templateInfoFormRef = ref();
const paramConfigRefs = ref([]);
const templateInfoForm = reactive({
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

  createPermissionExpTemplate(templateInfoForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetTemplateInfoForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建权限表达式模板");
    });
};

export default defineComponent({
  components: {
    ParamSelect,
  },
  setup() {
    return {
      handleBack,
      templateInfoForm,
      templateInfoFormRules,
      templateInfoFormRef,
      paramConfigRefs,
      addParamConfigModalVisible,
      parmaTypes,
      selectedParamType,
      handleParamConfigModalOpen,
      handleParamConfigModalClose,
      handleParamConfigModalConfirm,
      handleRemoveParamConfig,
      handleTemplateInfoFormSubmit,
      handleResetTemplateInfoForm,
    };
  },
});
