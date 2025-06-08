import {defineComponent, reactive, ref} from "vue";
import router from "@/router";
import {generateRandomString, handleApiError, handleApiSuccess,} from "@/util/tool";
import {createUser} from "@/api/user";
import {Notification} from "@arco-design/web-vue";
import {checkPasswordWithoutPolicy} from "@/api/setting";

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
  passwordCheckerRef.value.setPassword(generateRandomString(12));
};

/**
 * 提交创建用户表单
 */
const handleCreateUserFormSubmit = () => {
  if (!checkPasswordRes.valid) {
    return;
  }

  createUser(createUserForm)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("创建成功");
        handleResetCreateUserForm();
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
  passwordCheckerRef.value.setPassword("");
};

/**
 * 密码检查
 */
const passwordCheckerRef = ref(null);
const checkPasswordLoading = ref(false);
const checkPasswordRes = reactive({
  valid: false,
  errorMessage: undefined,
  ruleResults: undefined,
});
const handleCheckPassword = (password: string) => {
  checkPasswordLoading.value = true;
  createUserForm.password = password;
  checkPasswordWithoutPolicy({
    identity: createUserForm.username,
    password,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        checkPasswordRes.valid = data.valid;
        checkPasswordRes.errorMessage = data.errorMessage;
        if (data.ruleResults) {
          checkPasswordRes.ruleResults = data.ruleResults;
        } else {
          checkPasswordRes.ruleResults = [];
        }
        checkPasswordLoading.value = false;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "密码检查");
      checkPasswordLoading.value = false;
    });
};

export default defineComponent({
  setup() {
    return {
      handleBack,
      createUserForm,
      createUserFormRules,
      createUserFormRef,
      handleGeneratePassword,
      handleCreateUserFormSubmit,
      handleResetCreateUserForm,
      passwordCheckerRef,
      checkPasswordLoading,
      checkPasswordRes,
      handleCheckPassword,
    };
  },
});
