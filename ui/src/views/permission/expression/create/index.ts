import {defineComponent, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {
    createPermissionExp,
    getPermissionExpTemplateList,
    getPremissionExpTemplateParamConfigs,
} from "@/api/permission";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Notification} from "@arco-design/web-vue";
import ParamInput from "../components/ParamInput.vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 模板列表 */
const templateList = reactive([]);
const handleGetTemplateList = () => {
  getPermissionExpTemplateList({
    page: 1,
    size: -1,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        templateList.length = 0;
        templateList.push(...data.list);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取限制条件模板列表");
    });
};

/** 模板参数配置 */
const templateParamConfigs = reactive([]);
const handleTemplateSelectChange = async (templateId: string) => {
  if (!templateId) {
    templateParamConfigs.length = 0;
    createPermissionExpInfoForm.templateParams = undefined;
    return;
  }
  await handleGetParamConfigs(templateId);
};
const handleGetParamConfigs = async (templateId: string) => {
  try {
    const result = await getPremissionExpTemplateParamConfigs(templateId);
    handleApiSuccess(result, (data: any) => {
      templateParamConfigs.length = 0;
      templateParamConfigs.push(...data);

      createPermissionExpInfoForm.templateParams = data.map((item: any) => {
        return {
          code: item.code,
          value: item.defaultValue,
        };
      });
    });
  } catch (err: any) {
    handleApiError(err, "获取限制条件模板参数配置");
  }
};

/** 创建限制条件表单 */
const templateParamsRef = ref(null);
const createPermissionExpInfoForm = reactive({
  name: undefined,
  expression: undefined,
  templateId: undefined,
  templateParams: undefined,
  desc: undefined,
});
const createPermissionExpInfoFormRef = ref();
const createPermissionExpInfoFormRules = {
  name: [{ required: true, message: "限制条件名称未填写" }],
  expression: [{ required: true, message: "JEXL 表达式未填写" }],
};

/**
 * 提交创建限制条件表单
 */
const handleCreatePermissionExpInfoFormSubmit = async () => {
  const validateResults = [];
  validateResults.push(createPermissionExpInfoFormRef.value.validate());

  const useTemplate = createPermissionExpInfoForm.templateId ? true : false;
  if (useTemplate) {
    validateResults.push(templateParamsRef.value.validate());
  }
  const result = await Promise.all(validateResults);
  const isValid = result.filter((item) => item).length === 0;
  if (!isValid) {
    return;
  }

  createPermissionExp({
    ...createPermissionExpInfoForm,
    useTemplate,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreatePermissionExpInfoForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建限制条件");
    });
};

/**
 * 重置创建限制条件表单
 */
const handleResetCreatePermissionExpInfoForm = () => {
  const templateId = createPermissionExpInfoForm.templateId;
  createPermissionExpInfoFormRef.value.resetFields();
  createPermissionExpInfoForm.templateId = templateId;

  if (templateId) {
    templateParamsRef.value.reset(); 
  }
};

export default defineComponent({
  components: {
    ParamInput,
  },
  setup() {
    onMounted(() => {
      handleGetTemplateList();
      createPermissionExpInfoForm.templateId = undefined;
      templateParamConfigs.length = 0;
    });

    return {
      handleBack,
      templateList,
      templateParamsRef,
      createPermissionExpInfoForm,
      createPermissionExpInfoFormRef,
      createPermissionExpInfoFormRules,
      handleCreatePermissionExpInfoFormSubmit,
      handleResetCreatePermissionExpInfoForm,
      templateParamConfigs,
      handleTemplateSelectChange,
    };
  },
});
