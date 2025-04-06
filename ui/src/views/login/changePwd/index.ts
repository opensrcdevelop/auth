import {changePwd} from "@/api/login";
import {getQueryString, handleApiError, handleApiSuccess} from "@/util/tool";
import {defineComponent, onMounted, reactive, ref} from "vue";
import router from "@/router";
import {Notification} from "@arco-design/web-vue";
import {logoutSubmit} from "@/api/logout";
import {checkPasswordWithoutPolicy} from "@/api/setting";

const type = ref("0");

const changePwdForm = reactive({
  rawPwd: "",
  newPwd: "",
  confirmPwd: "",
});

const changePwdFormRules = {
  rawPwd: [{ required: true, message: "原始密码未填写" }],
  newPwd: [{ required: true, message: "新密码未填写" }],
  confirmPwd: [
    { required: true, message: "确认新密码未填写" },
    {
      validator: (value, cb) => {
        if (value !== changePwdForm.newPwd) {
          cb("两次输入的密码不一致");
        } else {
          cb();
        }
      },
    },
  ],
};

/**
 * 提交变更密码表单
 *
 * @param formData 变更密码表单
 */
const handleChangePwdFormSubmit = (formData: any) => {
  if (!checkRes.valid) {
    return;
  }

  changePwd({
    rawPwd: formData.rawPwd,
    newPwd: formData.newPwd,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("修改密码成功");
        router.push({
          path: "/",
        });
      });
    })
    .catch((err: any) => {
      handleApiError(err, "修改密码");
      // 登出
      logoutSubmit()
        .then((result: any) => {
          handleApiSuccess(result, () => {
            router.push({
              path: "/oauth2/redirect",
            });
          });
        })
        .catch((err: any) => {
          handleApiError(err, "登出");
        });
    });
};

/**
 * 密码检查
 */
const checkLoading = ref(false);
const checkRes = reactive({
  valid: false,
  errorMessage: undefined,
  ruleResults: undefined,
});
const handleCheckPassword = (password: string) => {
  checkLoading.value = true;
  changePwdForm.newPwd = password;
  checkPasswordWithoutPolicy({
    password,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        checkRes.valid = data.valid;
        checkRes.errorMessage = data.errorMessage;
        if (data.ruleResults) {
          checkRes.ruleResults = data.ruleResults;
        } else {
          checkRes.ruleResults = [];
        }
        checkLoading.value = false;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "密码检查");
      checkLoading.value = false;
    });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      type.value = getQueryString("type") || "0";
    })

    return {
      type,
      changePwdForm,
      changePwdFormRules,
      handleChangePwdFormSubmit,
      checkLoading,
      checkRes,
      handleCheckPassword,
    };
  },
});
