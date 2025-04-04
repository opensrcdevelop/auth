import {
    deletePasswordPolicy,
    getPasswordPolicyList,
    updatePasswordPolicy,
    updatePasswordPolicyPriority,
} from "@/api/setting";
import router from "@/router";
import {handleApiError, handleApiSuccess} from "@/util/tool";
import {Modal, Notification} from "@arco-design/web-vue";
import {defineComponent, onMounted, reactive, ref} from "vue";

const activeTab = ref("password_policy");

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
 * 跳转到创建密码策略页面
 */
const handleToCreatePasswordPolicy = () => {
  router.push({
    path: "/system_setting/password/create",
  });
};

/**
 * 密码策略列表
 */
const passwordPolicyList = reactive([]);

const handleGetPasswordPolicyList = () => {
  getPasswordPolicyList()
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        passwordPolicyList.length = 0;
        passwordPolicyList.push(...data);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取密码策略列表");
    });
};

/**
 * 获取密码强度标签
 */
const getPasswordStrengthLabel = (strength: number) => {
  let label = "";
  switch (strength) {
    case 0:
      label = "无要求";
      break;
    case 1:
      label = "低强度";
      break;
    case 2:
      label = "中强度";
      break;
    case 3:
      label = "高强度";
      break;
    case 4:
      label = "自定义强度";
      break;
  }
  return label;
};

/**
 * 更新密码策略执行顺序
 */
const handlTableChange = async (data: []) => {
  passwordPolicyList.length = 0;
  passwordPolicyList.push(...data);

  const requestData = data.map((item: any, index) => {
    return {
      id: item.id,
      priority: index,
    };
  });
  try {
    await updatePasswordPolicyPriority(requestData);
    Notification.success("更新执行顺序成功");
  } catch (err: any) {
    handleApiError(err, "更新执行顺序");
  }
  handleGetPasswordPolicyList();
};

/**
 * 更新密码策略状态
 */
const handleUpdatePasswordPolicyState = (data: any) => {
  updatePasswordPolicy({
    id: data.id,
    enabled: data.enabled,
  })
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("更新密码策略状态成功");
        handleGetPasswordPolicyList();
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新密码策略状态");
      handleGetPasswordPolicyList();
    });
};

/**
 * 跳转密码策略详情
 */
const handleToPasswordPolicyDetail = (passwordPolicy: any) => {
  router.push({
    path: "/system_setting/password/detail",
    query: {
      id: passwordPolicy.id,
    },
  });
};

/**
 * 删除角色
 */
const handleDeletePasswordPolicy = (passwordPolicy: any) => {
  Modal.warning({
    title: `确定删除密码策略「${passwordPolicy.name}」吗？`,
    content: "",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deletePasswordPolicy(passwordPolicy.id)
        .then((result: any) => {
          Notification.success("删除成功");
          handleGetPasswordPolicyList();
        })
        .catch((err: any) => {
          handleApiError(err, "删除密码策略");
        });
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetPasswordPolicyList();
    });

    return {
      activeTab,
      handleTabChange,
      handleToCreatePasswordPolicy,
      passwordPolicyList,
      getPasswordStrengthLabel,
      handlTableChange,
      handleUpdatePasswordPolicyState,
      handleToPasswordPolicyDetail,
      handleDeletePasswordPolicy,
    };
  },
});
