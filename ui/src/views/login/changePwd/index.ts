import { changePwd } from "@/api/login";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, reactive } from "vue";
import router from "@/router";
import { Notification } from "@arco-design/web-vue";
import { logoutSubmit } from "@/api/logout";

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

export default defineComponent({
  setup() {
    return {
      changePwdForm,
      changePwdFormRules,
      handleChangePwdFormSubmit,
    };
  },
});
