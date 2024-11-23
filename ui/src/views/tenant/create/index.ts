import { createTenant } from "@/api/tenant";
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

/** 创建租户表单 */
const createTenantInfoForm = reactive({
  name: undefined,
  code: undefined,
  desc: undefined,
});
const createTenantInfoFormRef = ref();
const createTenantInfoFormRules = {
  name: [{ required: true, message: "租户名称未填写" }],
  code: [
    { required: true, message: "租户标识未填写" },
    {
      validator: (value, cb) => {
        if (value && !/^[a-z0-9]+$/.test(value)) {
          cb("只允许包含小写英文字母、数字");
        } else {
          cb();
        }
      },
    },
  ],
};

/**
 * 重置创建租户表单
 */
const handleResetCreateTenantInfoForm = () => {
  createTenantInfoFormRef.value.resetFields();
};

/**
 * 提交创建租户表单
 *
 * @param formData 创建租户表单
 */
const handleCreateTenantInfoFormSubmit = (formData: any) => {
  createTenant(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateTenantInfoForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建租户");
    });
};

/**
 * 生成租户标识
 * 
 * @returns 租户标识
 */
const handleGenerateTenantCode = () => {
  let text = "";
  const possible =
    "abcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < 8; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }
  createTenantInfoForm.code = text;
}

export default defineComponent({
  setup() {
    return {
      handleBack,
      createTenantInfoFormRef,
      createTenantInfoForm,
      createTenantInfoFormRules,
      handleResetCreateTenantInfoForm,
      handleCreateTenantInfoFormSubmit,
      handleGenerateTenantCode
    };
  },
});
