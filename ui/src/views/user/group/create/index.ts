import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { createUserGroup } from "@/api/userGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Notification } from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

/**
 * 创建用户组表单
 */
const createUserGroupForm = reactive({
  name: "",
  code: "",
  desc: "",
});
const createUserGroupFormRef = ref();
const createUserGroupFormRules = {
  name: [{ required: true, message: "用户组名称未填写" }],
  code: [
    { required: true, message: "用户组标识未填写" },
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
 * 重置创建用户组表单
 */
const handleResetCreateUserGroupForm = () => {
  createUserGroupFormRef.value.resetFields();
};

/**
 * 提交创建用户组表单
 * 
 * @param formData 创建用户组表单
 */
const handleCreateUserGroupFormSubmit = (formData: any) => {
  createUserGroup(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateUserGroupForm();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建用户组");
    });
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createUserGroupForm,
      createUserGroupFormRef,
      createUserGroupFormRules,
      handleResetCreateUserGroupForm,
      handleCreateUserGroupFormSubmit
    };
  },
});
