import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createPermissionExp } from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 创建限制条件表单 */
const createPermissionExpInfoForm = reactive({
  name: undefined,
  expression: undefined,
  desc: undefined,
});
const createPermissionExpInfoFormRef = ref();
const createPermissionExpInfoFormRules = {
  name: [{ required: true, message: "限制条件名称未填写" }],
  expression: [{ required: true, message: "SpringEL 表达式未填写" }],
};

/**
 * 提交创建限制条件表单
 */
const handleCreatePermissionExpInfoFormSubmit = (formData: any) => {
  createPermissionExp(formData)
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
  createPermissionExpInfoFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createPermissionExpInfoForm,
      createPermissionExpInfoFormRef,
      createPermissionExpInfoFormRules,
      handleCreatePermissionExpInfoFormSubmit,
      handleResetCreatePermissionExpInfoForm,
    };
  },
});
