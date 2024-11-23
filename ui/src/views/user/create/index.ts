import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import {
  generateRandomString,
  handleApiError,
  handleApiSuccess,
} from "@/util/tool";
import { createUser } from "@/api/user";
import { Notification } from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  handleResetCreateUserForm();
  router.back();
};

/** 创建用户表单 */
const createUserForm = reactive({
  username: null,
  password: null,
  phoneNumber: null,
  emailAddress: null,
  needChangePwd: false,
  sendEmail: false,
});

const createUserFormRef = ref();

const createUserFormRules = {
  username: [
    {
      required: true,
      message: "用户名未填写",
    },
  ],
  password: [
    {
      required: true,
      message: "密码未填写",
    },
  ],
  emailAddress: [
    {
      validator: (value, cb) => {
        if (createUserForm.sendEmail && !value) {
          cb("邮箱未填写");
        } else {
          cb();
        }
      },
    },
  ],
};

/**
 * 生成随机密码
 */
const handleGeneratePassword = () => {
  createUserForm.password = generateRandomString(12);
};

/**
 * 提交创建用户表单
 */
const handleCreateUserFormSubmit = () => {
  createUser(createUserForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        createUserFormRef.value.resetFields();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "创建用户");
    });
};

/**
 * 重置创建用户表单
 */
const handleResetCreateUserForm = () => {
  createUserFormRef.value.resetFields();
}

export default defineComponent({
  setup() {
    return {
      handleBack,
      createUserForm,
      createUserFormRules,
      createUserFormRef,
      handleGeneratePassword,
      handleCreateUserFormSubmit,
      handleResetCreateUserForm
    };
  },
});
