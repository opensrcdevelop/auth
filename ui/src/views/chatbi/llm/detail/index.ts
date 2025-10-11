import router from "@/router";
import {defineComponent, onMounted, reactive, ref} from "vue";
import {MODEL_PROVIDER_TYPE_LIST} from "../constants";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {getModelProviderDetail, updateModelProvider} from "@/api/chatbi";
import {Modal, Notification} from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("model_provider_info");

const handleTabInit = (tabKey: string, id: string = modelProviderId.value) => {
  switch (tabKey) {
    case "model_provider_info":
      handleGetModelProviderDetail(id);
      break;
  }
};

const modelProviderTypeList = MODEL_PROVIDER_TYPE_LIST;

const modelProviderId = ref("");
const modelProviderName = ref("");

/**
 * 模型提供商信息
 */
const modelProviderInfoFormRef = ref();
const modelProviderInfoFormRules = {
  name: [{ required: true, message: "模型提供商名称未填写" }],
  baseUrl: [{ required: true, message: "API 接入地址未填写" }],
  defaultModel: [{ required: true, message: "默认模型未选择" }],
};
const modelProviderInfoForm = reactive({
  id: undefined,
  name: undefined,
  type: undefined,
  baseUrl: undefined,
  apiKey: undefined,
  temperature: undefined,
  maxTokens: undefined,
  defaultModel: undefined,
});

/**
 * 可选模型
 */
const optionalModelList = ref([]);

/**
 * 获取模型提供商详情
 */
const handleGetModelProviderDetail = (id: string = modelProviderId.value) => {
  getModelProviderDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        modelProviderId.value = data.id;
        modelProviderName.value = data.name;

        modelProviderInfoForm.id = data.id;
        modelProviderInfoForm.name = data.name;
        modelProviderInfoForm.type = data.type;
        modelProviderInfoForm.baseUrl = data.baseUrl;
        modelProviderInfoForm.apiKey = data.apiKey;
        modelProviderInfoForm.temperature = data.temperature;
        modelProviderInfoForm.maxTokens = data.maxTokens;
        modelProviderInfoForm.defaultModel = data.defaultModel;

        optionalModelList.value = data.optionalModels;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取模型提供商详情");
    });
};

/**
 * 提交更新模型提供商信息表单
 */
const handleModelProviderInfoFormSubmit = (formData: any) => {
  updateModelProvider(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetModelProviderDetail();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新模型提供商信息");
    });
};

/**
 * 重置模型提供商信息表单
 */
const handleResetModelProviderInfoForm = () => {
  modelProviderInfoFormRef.value.resetFields();
  handleGetModelProviderDetail();
};

/**
 * 可选模型列表变更
 */
const handleOptionalModelListChange = (_data) => {
  optionalModelList.value = _data;
  handleUpdateOptionalModelList();
};

/**
 * 移除可选模型
 */
const handleRemoveOptionalModel = (model: any) => {
  Modal.warning({
    title: `确定删除模型「${model.name}」吗？`,
    content: "此操作将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      optionalModelList.value.splice(optionalModelList.value.indexOf(model), 1);
      handleUpdateOptionalModelList();
    },
  });
};

/**
 * 更新可选模型列表
 */
const handleUpdateOptionalModelList = () => {
  updateModelProvider({
    id: modelProviderId.value,
    optionalModels: optionalModelList.value.map((item: any) => item.name),
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("更新可选模型列表成功");
        handleGetModelProviderDetail();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新可选模型列表");
    });
};

/**
 * 添加可选模型对话框
 */
const addOptionalModelModalVisible = ref(false);
const addOptionalModelForm = reactive({
  name: undefined,
});
const addOptionalModelFormRef = ref();
const addOptionalModelFormRules = {
  name: [{ required: true, message: "模型名称未填写" }],
};

/**
 * 提交添加可选模型表单
 */
const handleAddOptionalModelFormSubmit = async () => {
  const errors = await addOptionalModelFormRef.value.validate();
  if (!errors) {
    optionalModelList.value.push({
      name: addOptionalModelForm.name,
      usedReqTokens: 0,
      usedRepTokens: 0,
    });
    addOptionalModelModalVisible.value = false;
    handleUpdateOptionalModelList();
  } else {
    addOptionalModelModalVisible.value = true;
  }
};

/**
 * 关闭添加可选模型对话框
 */
const handleCloseAddOptionalModelModal = () => {
  addOptionalModelModalVisible.value = false;
  addOptionalModelFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    const modelProviderId = getQueryString("id");

    onMounted(() => {
      activeTab.value = getQueryString("active_tab") || "model_provider_info";
      handleTabInit(activeTab.value, modelProviderId);
    });

    return {
      handleBack,
      activeTab,
      modelProviderTypeList,
      modelProviderId,
      modelProviderName,
      modelProviderInfoFormRef,
      modelProviderInfoForm,
      modelProviderInfoFormRules,
      optionalModelList,
      handleModelProviderInfoFormSubmit,
      handleResetModelProviderInfoForm,
      handleOptionalModelListChange,
      addOptionalModelModalVisible,
      addOptionalModelForm,
      addOptionalModelFormRef,
      addOptionalModelFormRules,
      handleAddOptionalModelFormSubmit,
      handleRemoveOptionalModel,
      handleCloseAddOptionalModelModal,
    };
  },
});
