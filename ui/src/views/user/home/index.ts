import {changePwd} from "@/api/login";
import {logoutSubmit} from "@/api/logout";
import {checkPasswordWithoutPolicy} from "@/api/setting";
import {getCurrentUser} from "@/api/user";
import router from "@/router";
import {AUTH_TOKENS} from "@/util/constants";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";
import {useRoute} from "vue-router";
import MyUserInfo from "./components/MyUserInfo.vue";
import AccountBinding from "./components/AccountBinding.vue";
import {useGlobalVariablesStore} from "@/store/globalVariables";
import MyPermissions from "./components/MyPermissions.vue";
import ApplyPermission from "./components/ApplyPermission.vue";
import RequestRecords from "./components/RequestRecords.vue";

const globalVariables = useGlobalVariablesStore();

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
 * 用户信息更新后，更新父组件的用户信息
 */
const handleUserInfoUpdated = (data: any) => {
  username.value = data.username;
  consoleAccess.value = data.consoleAccess;
  Object.assign(userInfo, data);
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
      validator: (value: any, cb: any) => {
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
const passwordCheckerRef = ref();
const checkPasswordLoading = ref(false);
const checkPasswordRes = reactive({
  valid: false,
  errorMessage: undefined,
  ruleResults: undefined as any,
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

export default defineComponent({
  components: {
    MyUserInfo,
    AccountBinding,
    MyPermissions,
    ApplyPermission,
    RequestRecords,
  },
  setup() {
    onMounted(() => {
      handleGetUserInfo();

      const route = useRoute();
      if (route.query.active_tab) {
        activeTab.value = route.query.active_tab as string;
      }
    });

    return {
      globalVariables,
      activeTab,
      handleTabChange,
      handleToConsole,
      handleLogout,
      handleUserInfoUpdated,
      username,
      userInfo,
      userAttrs,
      changePwdModalVisible,
      handleOpenChangePwdModal,
      handleCloseChangePwdModal,
      changePwdForm,
      changePwdFormRef,
      changePwdFormRules,
      handleSubmitChangePwdForm,
      changePwdFormSubmitLoading,
      allDictDatas,
      passwordCheckerRef,
      checkPasswordLoading,
      checkPasswordRes,
      handleCheckPassword,
      consoleAccess,
    };
  },
});
