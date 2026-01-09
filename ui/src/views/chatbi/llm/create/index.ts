import router from "@/router";
import {computed, defineComponent, reactive, ref} from "vue";
import {MODEL_PROVIDER_TYPE_LIST} from "../constants";
import {createModelProvider} from "@/api/chatbi";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const modelProviderTypeList = MODEL_PROVIDER_TYPE_LIST;

/**
 * 创建模型提供商表单
 */
const createModelProviderFormRef = ref();
const createModelProviderFormRules = {
  name: [{ required: true, message: "模型提供商名称未填写" }],
  type: [{ required: true, message: "模型提供商类型未选择" }],
  baseUrl: [{ required: true, message: "API 接入地址未填写" }],
  optionalModels: [{ required: true, message: "可选模型未填写" }],
  defaultModel: [{ required: true, message: "默认模型未选择" }],
};
const createModelProviderForm = reactive({
  name: undefined,
  type: undefined,
  baseUrl: undefined,
  apiKey: undefined,
  temperature: undefined,
  maxTokens: undefined,
  defaultModel: undefined,
  optionalModels: undefined,
});

/**
 * 可选模型
 */
const selectableModels = computed(() => {
  return createModelProviderForm.optionalModels
    ?.split("\n")
    .filter((item: string) => item !== "");
});

/**
 * 提交模型提供商表单
 */
const handleCreateModelProviderFormSubmit = (formData: any) => {
  createModelProvider({
    name: formData.name,
    type: formData.type,
    baseUrl: formData.baseUrl,
    apiKey: formData.apiKey,
    temperature: formData.temperature,
    maxTokens: formData.maxTokens,
    defaultModel: formData.defaultModel,
    optionalModels: createModelProviderForm.optionalModels
      ?.split("\n")
      .filter((item: string) => item !== ""),
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateModelProviderForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建模型提供商");
    });
};

/**
 * 重置模型提供商表单
 */
const handleResetCreateModelProviderForm = () => {
  createModelProviderFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      modelProviderTypeList,
      createModelProviderFormRef,
      createModelProviderFormRules,
      createModelProviderForm,
      selectableModels,
      handleCreateModelProviderFormSubmit,
      handleResetCreateModelProviderForm,
    };
  },
});
