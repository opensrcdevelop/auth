import {getEnabledDictData} from "@/api/dict";
import {bindUser, getBoundIdentitySource, unbindUser,} from "@/api/identitySource";
import {changePwd, sendEmailCodeSubmit} from "@/api/login";
import {logoutSubmit} from "@/api/logout";
import {checkPasswordWithoutPolicy} from "@/api/setting";
import {
    bindEmail,
    getCurrentUser,
    getVisibleUserAttrs,
    sendBindEmailCode,
    unbindEmail,
    updateMyUserInfo,
} from "@/api/user";
import router from "@/router";
import {AUTH_FAILURE, AUTH_SUCCESS, AUTH_TOKENS, BINDING_EXISTS,} from "@/util/constants";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Message, Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";
import {useRoute} from "vue-router";
import webauthn from "@/util/webauthn";
import {
    completeWebAuthnRegistration,
    deleteWebAuthnCredential,
    getWebAuthnRegisterOptions,
    listWebAuthnCredentials,
} from "@/api/webauthn";

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
  handleTabInit(tabKey);
};

const loading = ref(false);
const handleTabInit = (tabKey: string) => {
  loading.value = true;
  const initPromises = [];

  switch (tabKey) {
    case "user_info":
      initPromises.push(handleGetUserInfo());
      initPromises.push(handleGetUserAttrs());
      break;
    case "account_binding":
      initPromises.push(handleGetUserInfo());
      initPromises.push(handleGetBoundIdentitySource());
      initPromises.push(handleGetWebAuthnCredentials());
      break;
  }

  if (initPromises.length > 0) {
    Promise.all(initPromises).finally(() => {
      loading.value = false;
    });
  } else {
    loading.value = false;
  }
};

// 控制台访问权限
const consoleAccess = ref(false);

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
            localStorage.removeItem(AUTH_TOKENS);

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

/** 字典数据值 */
const allDictDatas = reactive({});

/**
 * 获取用户信息
 */
const handleGetUserInfo = () => {
  getCurrentUser()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        username.value = data.username;
        consoleAccess.value = data.consoleAccess;

        Object.assign(userInfo, data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户信息");
    });
};

/**
 * 获取用户属性
 */
const handleGetUserAttrs = async () => {
  await getVisibleUserAttrs()
    .then((result: any) => {
      handleApiSuccess(result, async (data: any) => {
        userAttrs.length = 0;
        userAttrs.push(...data);

        // 将用户 ID 置为第一个属性
        const userIdIndex = userAttrs.findIndex(
          (item: any) => item.key === "userId",
        );
        if (userIdIndex > -1) {
          userAttrs.splice(0, 0, userAttrs.splice(userIdIndex, 1)[0]);
        }

        // 将用户名置为第二个属性
        const userNameIndex = userAttrs.findIndex(
          (item: any) => item.key === "username",
        );
        if (userNameIndex > -1) {
          userAttrs.splice(1, 0, userAttrs.splice(userNameIndex, 1)[0]);
        }

        // 将邮箱置为第三个属性
        const emailIndex = userAttrs.findIndex(
          (item: any) => item.key === "emailAddress",
        );
        if (emailIndex > -1) {
          userAttrs.splice(2, 0, userAttrs.splice(emailIndex, 1)[0]);
        }

        // 将手机号置为第四个属性
        const phoneIndex = userAttrs.findIndex(
          (item: any) => item.key === "phoneNumber",
        );
        if (phoneIndex > -1) {
          userAttrs.splice(3, 0, userAttrs.splice(phoneIndex, 1)[0]);
        }
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取可见的用户属性");
    });

  handleGetAllEnabledDictData();
};

/**
 * 获取所有启用的字典数据
 */
const handleGetAllEnabledDictData = async () => {
  const getEnabledDictDataPromises = [];
  userAttrs.forEach((item: any) => {
    if (item.dataType === "DICT" && item.dictId) {
      allDictDatas[item.key] = [];
      getEnabledDictDataPromises.push(
        handleGetEnabledDictData(item.key, item.dictId),
      );
    }
  });

  if (getEnabledDictDataPromises.length > 0) {
    await Promise.all(getEnabledDictDataPromises);
  }
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
  loading.value = true;
  updateMyUserInfo(userInfo)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetUserAttrs();
        handleGetUserInfo();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新个人信息");
    })
    .finally(() => {
      loading.value = false;
    });
};

/** 修改密码对话框 */
const changePwdModalVisible = ref(false);
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
  changePwdModalVisible.value = true;
};

/**
 * 关闭修改密码对话框
 */
const handleCloseChangePwdModal = () => {
  changePwdModalVisible.value = false;
  changePwdFormRef.value.resetFields();
  passwordCheckerRef.value.setPassword("");
};

/**
 * 提交修改密码表单
 *
 * @param formData 修改密码表单
 */
const handleSubmitChangePwdForm = (formData: any) => {
  if (!checkPasswordRes.valid) {
    return;
  }

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
  changePwdForm.newPwd = password;
  checkPasswordWithoutPolicy({
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

/** 绑定 / 解绑邮箱对话框 */
const bindOrUnbindEmailModalVisible = ref(false);
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
  bindOrUnbindEmailModalVisible.value = true;
};

/**
 * 打开解绑邮箱对话框
 */
const handleOpenUnbindEmailModal = () => {
  isBinding.value = false;
  bindOrUnbindEmailForm.email = userInfo["emailAddress"];
  bindOrUnbindEmailModalVisible.value = true;
};

/**
 * 关闭绑定 / 解绑邮箱对话框
 */
const handleCoseBindOrUnbindEmailModal = () => {
  bindOrUnbindEmailModalVisible.value = false;
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

/** 绑定的身份源 */
const boundIdentitySource = reactive([]);

/** WebAuthn 凭证列表 */
const webAuthnCredentials = reactive([]);

/** 是否支持 WebAuthn */
const isWebAuthnSupported = ref(false);

/** 是否正在添加凭证 */
const addingWebAuthnCredential = ref(false);

/**
 * 获取 WebAuthn 凭证列表
 */
const handleGetWebAuthnCredentials = () => {
  listWebAuthnCredentials()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        webAuthnCredentials.length = 0;
        webAuthnCredentials.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取 WebAuthn 凭证");
    });
};

/**
 * 添加 WebAuthn 凭证
 */
const handleAddWebAuthnCredential = () => {
  // 检查浏览器是否支持 WebAuthn
  if (!webauthn.isSupported()) {
    Notification.warning("当前浏览器不支持 Passkey");
    return;
  }

  addingWebAuthnCredential.value = true;
  getWebAuthnRegisterOptions()
    .then((result: any) => {
      handleApiSuccess(result, async (data: any) => {
        try {
          const credential = await webauthn.startRegistration(data);
          await completeWebAuthnRegistration({
            id: credential.id,
            rawId: credential.rawId,
            response: {
              clientDataJSON: credential.response.clientDataJSON,
              attestationObject: credential.response.attestationObject,
            },
            transports: credential.response.transports?.join(",") || "",
          });
          Notification.success("添加 Passkey 凭证成功");
          handleGetWebAuthnCredentials();
        } catch (error: any) {
          if (error.message && error.message.includes("not allowed")) {
            Notification.warning("已取消添加 Passkey 凭证");
          } else if (error.message.includes("previously registered")) {
            Notification.warning("该设备已注册过 Passkey，无法重复注册");
          } else {
            Notification.error(
              "添加凭证失败: " + (error.message || "未知错误"),
            );
          }
        }
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取注册选项");
    })
    .finally(() => {
      addingWebAuthnCredential.value = false;
    });
};

/**
 * 删除 WebAuthn 凭证
 */
const handleDeleteWebAuthnCredential = (credential: any) => {
  deleteWebAuthnCredential(credential.id)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("删除凭证成功");
        handleGetWebAuthnCredentials();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "删除凭证");
    });
};

/**
 * 获取绑定的身份源
 */
const handleGetBoundIdentitySource = () => {
  getBoundIdentitySource()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        boundIdentitySource.length = 0;
        boundIdentitySource.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取绑定的身份源");
    });
};

/**
 * 绑定第三方账号
 */
var authWindow;
const handleBindUser = (identitySource: any) => {
  loading.value = true;
  bindUser(identitySource.code)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        // 打开第三方认证窗口
        authWindow = window.open(
          data.authReqUri,
          "_blank",
          "width=600,height=600",
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "绑定第三方账号");
    })
    .finally(() => {
      loading.value = false;
    });
};

/**
 * 处理第三方认证窗口响应
 */
const handleAuthWindowResponse = (event) => {
  if (event.data === AUTH_SUCCESS) {
    Notification.success("第三方账号绑定成功");
    authWindow.close();
    handleGetBoundIdentitySource();
  }

  if (event.data === AUTH_FAILURE) {
    Notification.error("第三方账号绑定失败");
    authWindow.close();
  }

  if (event.data === BINDING_EXISTS) {
    Notification.error("该第三方账号已绑定其他用户，请先解绑");
    authWindow.close();
  }
};

/**
 * 解绑第三方账号
 */
const handleUnbindUser = (identitySource: any) => {
  Modal.warning({
    title: `确定与「${identitySource.name}」解除绑定吗？`,
    content: "",
    hideCancel: false,
    okButtonProps: {
      status: "warning",
    },
    onOk: () => {
      unbindUser(identitySource.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("解绑成功");
            handleGetBoundIdentitySource();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "绑定第三方账号");
        });
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
      handleTabInit(activeTab.value);
      window.addEventListener("message", handleAuthWindowResponse);

      // 检测浏览器是否支持 WebAuthn
      isWebAuthnSupported.value = webauthn.isSupported();
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
      loading,
      changePwdModalVisible,
      handleOpenChangePwdModal,
      handleCloseChangePwdModal,
      changePwdForm,
      changePwdFormRef,
      changePwdFormRules,
      handleSubmitChangePwdForm,
      changePwdFormSubmitLoading,
      bindOrUnbindEmailModalVisible,
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
      passwordCheckerRef,
      checkPasswordLoading,
      checkPasswordRes,
      handleCheckPassword,
      consoleAccess,
      boundIdentitySource,
      handleBindUser,
      handleUnbindUser,
      webAuthnCredentials,
      isWebAuthnSupported,
      addingWebAuthnCredential,
      handleAddWebAuthnCredential,
      handleDeleteWebAuthnCredential,
    };
  },
});
