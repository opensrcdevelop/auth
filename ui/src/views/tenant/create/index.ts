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
  effectiveTime: undefined,
  expirationTime: undefined,
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
  effectiveTime: [
    {
      validator: (value: any, cb: any) => {
        const expirationTime = createTenantInfoForm.expirationTime;
        // 如果生效时间为空，或者失效时间为空，或者生效时间早于失效时间，则通过
        if (!value || !expirationTime || new Date(value) <= new Date(expirationTime)) {
          cb();
        } else {
          cb("生效时间必须早于失效时间");
        }
      },
      trigger: "change",
    },
  ],
  expirationTime: [
    {
      validator: (value: any, cb: any) => {
        const effectiveTime = createTenantInfoForm.effectiveTime;
        // 如果生效时间为空，或者失效时间为空，或者生效时间早于失效时间，则通过
        if (!effectiveTime || !value || new Date(effectiveTime) <= new Date(value)) {
          cb();
        } else {
          cb("生效时间必须早于失效时间");
        }
      },
      trigger: "change",
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
};

/**
 * 生效时间或失效时间变化时触发验证
 */
const handleTimeChange = () => {
  createTenantInfoFormRef.value.validateField(["effectiveTime", "expirationTime"]);
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createTenantInfoFormRef,
      createTenantInfoForm,
      createTenantInfoFormRules,
      handleResetCreateTenantInfoForm,
      handleCreateTenantInfoFormSubmit,
      handleGenerateTenantCode,
      handleTimeChange,
    };
  },
});
