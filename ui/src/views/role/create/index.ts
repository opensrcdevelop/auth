import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createRole } from "@/api/role";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/** 创建角色表单 */
const createRoleFormRef = ref();
const createRoleForm = reactive({
  name: "",
  code: "",
  desc: "",
});
const createRoleFormRules = {
  name: [{ required: true, message: "角色名称未填写" }],
  code: [
    { required: true, message: "角色标识未填写" },
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
 * 重置创建角色表单
 */
const handleResetCreateRoleForm = () => {
  createRoleFormRef.value.resetFields();
};

/**
 * 提交创建角色表单
 */
const handleCreateRoleFormSubmit = (formData: any) => {
  createRole(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateRoleForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建角色");
    });
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createRoleFormRef,
      createRoleForm,
      createRoleFormRules,
      handleResetCreateRoleForm,
      handleCreateRoleFormSubmit,
    };
  },
});
