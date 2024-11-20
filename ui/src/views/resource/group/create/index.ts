import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createResourceGroup } from "@/api/resourceGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 创建资源组表单 */
const createResourceGroupFormRef = ref();
const createResourceGroupForm = reactive({
  name: undefined,
  code: undefined,
  desc: undefined,
});
const createResourceGroupFormRules = {
  name: [{ required: true, message: "资源组名称未填写" }],
  code: [
    { required: true, message: "资源组标识未填写" },
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
  resourceGroupId: [{ required: true, message: "资源组未选择" }],
};

/**
 * 提交创建资源组表单
 */
const handleCreateResourceGroupFormSubmit = (formData: any) => {
  createResourceGroup(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateResourceGroupForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建资源组");
    });
};

/**
 * 重置创建资源组表单
 */
const handleResetCreateResourceGroupForm = () => {
  createResourceGroupFormRef.value.resetFields();
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createResourceGroupFormRef,
      createResourceGroupForm,
      createResourceGroupFormRules,
      handleResetCreateResourceGroupForm,
      handleCreateResourceGroupFormSubmit,
    };
  },
});
