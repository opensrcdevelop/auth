import router from "@/router";
import {defineComponent, reactive, ref} from "vue";
import {MODEL_PROVIDER_TYPE_LIST} from "../constants";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const modelProviderTypeList = MODEL_PROVIDER_TYPE_LIST;

const createModelProviderFormRef = ref();
const createModelProviderFormRules = {
  name: [{ required: true, message: "模型提供商名称未填写" }],
  baseUrl: [{ required: true, message: "API 接入地址未填写" }],
  defaultModel: [{ required: true, message: "默认模型未选择" }],
};
const createModelProviderForm = reactive({
  id: undefined,
  name: undefined,
  type: undefined,
  baseUrl: undefined,
  apiKey: undefined,
  temperature: undefined,
  maxTokens: undefined,
  defaultModel: undefined,
  optionalModels: [],
});

const handleOptionalModelsChange = (val: any) => {
  createModelProviderForm.optionalModels.length = 0;
  createModelProviderForm.optionalModels.push(...val);
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      modelProviderTypeList,
      createModelProviderFormRef,
      createModelProviderFormRules,
      createModelProviderForm,
      handleOptionalModelsChange,
    };
  },
});
