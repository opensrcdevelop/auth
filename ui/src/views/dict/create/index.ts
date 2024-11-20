import { createDict } from "@/api/dict";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";
import { defineComponent, reactive, ref } from "vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 创建字典表单 */
const createDictFormRef = ref();
const createDictForm = reactive({
  name: undefined,
  code: undefined,
  desc: undefined,
});
const createDictFormRules = {
  name: [{ required: true, message: "字典名称未填写" }],
  code: [
    { required: true, message: "字典标识未填写" },
    {
      validator: (value, cb) => {
        if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
          cb("只允许包含英文字母、数字、下划线_、横线-");
        } else {
          cb();
        }
      },
    },
  ],
};

/**
 * 提交创建字典表单
 */
const handleCreateDictFormSubmit = (formData: any) => {
  createDict(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateDictForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建字典");
    });
};

/**
 * 重置创建字典表单
 */
const handleResetCreateDictForm = () => {
  createDictFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createDictFormRef,
      createDictForm,
      createDictFormRules,
      handleCreateDictFormSubmit,
      handleResetCreateDictForm,
    };
  },
});
