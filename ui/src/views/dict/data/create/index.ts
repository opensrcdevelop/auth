import { createDictData } from "@/api/dict";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 创建字典数据表单 */
const createDictDataFormRef = ref();
const createDictDataForm = reactive({
  label: undefined,
  value: undefined,
  enable: true,
  displaySeq: undefined,
  dictId: undefined,
});
const createDictDataFormRules = {
  label: [{ required: true, message: "数据标签未填写" }],
  value: [{ required: true, message: "数据值未填写" }],
  enable: [{ required: true, message: "是否启用未选择" }],
};

/**
 * 提交创建字典数据表单
 */
const handleCreateDictDataFormSubmit = (formData: any) => {
  createDictData(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateDictDataForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建字典数据");
    });
};

/**
 * 重置创建字典数据表单
 */
const handleResetCreateDictDataForm = () => {
  createDictDataFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const globalVariables = useGlobalVariablesStore().getData();
      createDictDataForm.dictId = globalVariables.dictId;
    });

    return {
      handleBack,
      createDictDataFormRef,
      createDictDataForm,
      createDictDataFormRules,
      handleCreateDictDataFormSubmit,
      handleResetCreateDictDataForm,
    };
  },
});
