import { getEnabledDictData } from "@/api/dict";
import { changePwd, sendEmailCodeSubmit } from "@/api/login";
import { logoutSubmit } from "@/api/logout";
import {
  bindEmail,
  getCurrentUser,
  getVisibleUserAttrs,
  sendBindEmailCode,
  unbindEmail,
  updateMyUserInfo,
} from "@/api/user";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Message, Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
import { useRoute } from "vue-router";

const activeTab = ref("user_info");

/**
 * tab 切换事件
 *
 * @param tabKey tabKey
 */
const handleTabChange = (tabKey: string) => {
  router.replace({
    query: {
      ...router.currentRoute.value.query,
      active_tab: tabKey,
    },
  });
  activeTab.value = tabKey;
};

/**
 * 跳转到控制台
 */
const handleToConsole = () => {
  router.push({
    path: "/",
  });
};

/**
 * 退出登录
 */
const handleLogout = () => {
  Modal.warning({
    title: "确定退出登录？",
    content: "",
    hideCancel: false,
    okButtonProps: {
      status: "warning",
    },
    onOk: () => {
      logoutSubmit()
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("退出成功");
            localStorage.removeItem("accessToken");

            // 跳转到登录页
            router.push({
              path: "/oauth2/redirect",
            });
          });
        })
        .catch((err: any) => {
          handleApiError(err, "退出登录");
        });
    },
  });
};

/** 用户名 */
const username = ref(undefined);
/** 用户信息 */
const userInfo = reactive({});
/** 用户属性 */
const userAttrs = reactive([]);
const userInfoLoading = ref(false);

/** 字典数据值 */
const allDictDatas = reactive({});

/**
 * 获取用户信息
 */
const handleGetUserInfo = () => {
  userInfoLoading.value = true;
  getCurrentUser()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        username.value = data.username;

        Object.assign(userInfo, data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户信息");
    })
    .finally(() => {
      userInfoLoading.value = false;
    });
};

/**
 * 获取用户属性
 */
const handleGetUserAttrs = () => {
  getVisibleUserAttrs()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userAttrs.length = 0;
        userAttrs.push(...data);

        // 将用户 ID 置为第一个属性
        const userIdIndex = userAttrs.findIndex(
          (item: any) => item.key === "userId"
        );
        if (userIdIndex > -1) {
          userAttrs.splice(0, 0, userAttrs.splice(userIdIndex, 1)[0]);
        }

        // 将用户名置为第二个属性
        const userNameIndex = userAttrs.findIndex(
          (item: any) => item.key === "username"
        );
        if (userNameIndex > -1) {
          userAttrs.splice(1, 0, userAttrs.splice(userNameIndex, 1)[0]);
        }

        // 将邮箱置为第三个属性
        const emailIndex = userAttrs.findIndex(
          (item: any) => item.key === "emailAddress"
        );
        if (emailIndex > -1) {
          userAttrs.splice(2, 0, userAttrs.splice(emailIndex, 1)[0]);
        }

        // 将手机号置为第四个属性
        const phoneIndex = userAttrs.findIndex(
          (item: any) => item.key === "phoneNumber"
        );
        if (phoneIndex > -1) {
          userAttrs.splice(3, 0, userAttrs.splice(phoneIndex, 1)[0]);
        }

        userAttrs.forEach((item: any) => {
          if (item.dataType === "DICT" && item.dictId) {
            allDictDatas[item.key] = [];
            handleGetEnabledDictData(item.key, item.dictId);
          }
        });
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取可见的用户属性");
    });
};

/**
 * 获取启用的字典数据
 */
const handleGetEnabledDictData = async (attrKey: string, dictId: string) => {
  try {
    const result = await getEnabledDictData(dictId);
    handleApiSuccess(result, (data: any) => {
      allDictDatas[attrKey].length = 0;
      allDictDatas[attrKey].push(...data);
    });
  } catch (err: any) {
    handleApiError(err, "获取启用的字典数据");
  }
};

/**
 * 更新个人信息
 */
const handleUpdateMyUserInfo = () => {
  if (!userInfo["username"] || userInfo["username"].trim() === "") {
    Message.warning("用户名不能为空");
    return;
  }
  userInfoLoading.value = true;
  updateMyUserInfo(userInfo)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新个人信息");
    })
    .finally(() => {
      userInfoLoading.value = false;
    });
};

/** 修改密码对话框 */
const changePwdModalVisivle = ref(false);
const changePwdForm = reactive({
  rawPwd: "",
  newPwd: "",
  confirmPwd: "",
});
const changePwdFormRef = ref();
const changePwdFormSubmitLoading = ref(false);
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
 * 打开修改密码对话框
 */
const handleOpenChangePwdModal = () => {
  changePwdModalVisivle.value = true;
};

/**
 * 关闭修改密码对话框
 */
const handleCloseChangePwdModal = () => {
  changePwdModalVisivle.value = false;
  changePwdFormRef.value.resetFields();
};

/**
 * 提交修改密码表单
 *
 * @param formData 修改密码表单
 */
const handleSubmitChangePwdForm = (formData: any) => {
  changePwdFormSubmitLoading.value = true;
  changePwd(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("密码修改成功");
        handleCloseChangePwdModal();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "修改密码");
    })
    .finally(() => {
      changePwdFormSubmitLoading.value = false;
    });
};

/** 绑定 / 解绑邮箱对话框 */
const bindOrUnbindEmailModalVisivle = ref(false);
const isBinding = ref(true);
const bindOrUnbindEmailFormSubmitLoading = ref(false);
const bindOrUnbindEmailFormRef = ref();
const bindOrUnbindEmailForm = reactive({
  email: undefined,
  code: undefined,
});
const bindOrUnbindEmailFormRules = {
  email: [
    {
      required: true,
      message: "邮箱未填写",
    },
  ],
  code: [
    {
      required: true,
      message: "验证码未填写",
    },
  ],
};

/**
 * 打开绑定邮箱对话框
 */
const handleOpenBindEmailModal = () => {
  isBinding.value = true;
  bindOrUnbindEmailModalVisivle.value = true;
};

/**
 * 打开解绑邮箱对话框
 */
const handleOpenUnbindEmailModal = () => {
  isBinding.value = false;
  bindOrUnbindEmailForm.email = userInfo["emailAddress"];
  bindOrUnbindEmailModalVisivle.value = true;
};

/**
 * 关闭绑定 / 解绑邮箱对话框
 */
const handleCoseBindOrUnbindEmailModal = () => {
  bindOrUnbindEmailModalVisivle.value = false;
  bindOrUnbindEmailFormRef.value.resetFields();
};

/**
 * 提交绑定 / 解绑邮箱表单
 */
const handleBindOrUnbindEmailFormSubmit = () => {
  bindOrUnbindEmailFormRef.value.validate(async (err) => {
    if (!err) {
      try {
        bindOrUnbindEmailFormSubmitLoading.value = true;
        if (isBinding.value) {
          await bindEmail(bindOrUnbindEmailForm);
          Notification.success("绑定邮箱成功");
          handleGetUserInfo();
        } else {
          await unbindEmail(bindOrUnbindEmailForm);
          Notification.success("解绑邮箱成功");
          handleGetUserInfo();
        }
        handleCoseBindOrUnbindEmailModal();
      } catch (err: any) {
        handleApiError(err, "绑定 / 解绑邮箱");
      } finally {
        bindOrUnbindEmailFormSubmitLoading.value = false;
      }
    }
  });
};

/** 发送邮箱验证码  */
const sendEmailCodeDisable = ref(false);
const sendEmailCodeBtnText = ref("发送验证码");
let remainingTime = 60;
let sendEmailCodeTimer;
const handleSendEmailCode = () => {
  if (!sendEmailCodeDisable.value) {
    bindOrUnbindEmailFormRef.value.validateField("email", async (err) => {
      if (!err) {
        try {
          if (isBinding.value) {
            await sendBindEmailCode(bindOrUnbindEmailForm.email);
          } else {
            await sendEmailCodeSubmit(bindOrUnbindEmailForm.email);
          }
          // 60s 倒计时
          sendEmailCodeDisable.value = true;
          sendEmailCodeBtnText.value = `${remainingTime}s 后重试`;
          sendEmailCodeTimer = setInterval(() => {
            remainingTime--;
            sendEmailCodeBtnText.value = `${remainingTime}s 后重试`;
            if (remainingTime < 0) {
              clearInterval(sendEmailCodeTimer);
              sendEmailCodeDisable.value = false;
              sendEmailCodeBtnText.value = "发送验证码";
              remainingTime = 60;
            }
          }, 1000);
        } catch (err) {
          handleApiError(err, "发送验证码");
        }
      }
    });
  }
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
      handleGetUserInfo();
      handleGetUserAttrs();
    });

    return {
      activeTab,
      handleTabChange,
      handleToConsole,
      handleLogout,
      username,
      userInfo,
      userAttrs,
      handleUpdateMyUserInfo,
      userInfoLoading,
      changePwdModalVisivle,
      handleOpenChangePwdModal,
      handleCloseChangePwdModal,
      changePwdForm,
      changePwdFormRef,
      changePwdFormRules,
      handleSubmitChangePwdForm,
      changePwdFormSubmitLoading,
      bindOrUnbindEmailModalVisivle,
      isBinding,
      bindOrUnbindEmailFormSubmitLoading,
      bindOrUnbindEmailFormRef,
      bindOrUnbindEmailForm,
      bindOrUnbindEmailFormRules,
      handleOpenBindEmailModal,
      handleOpenUnbindEmailModal,
      handleCoseBindOrUnbindEmailModal,
      handleBindOrUnbindEmailFormSubmit,
      sendEmailCodeDisable,
      sendEmailCodeBtnText,
      handleSendEmailCode,
      allDictDatas,
    };
  },
});
