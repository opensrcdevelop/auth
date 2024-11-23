import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import {
  getPermissionExpDetail,
  removeAuthorizeCondition,
  updatePermissionExp,
} from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { useRoute } from "vue-router";
import { Modal, Notification } from "@arco-design/web-vue";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("condition_info");

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

const permissionExpId = ref("");
const permissionExpName = ref("");

/** 权限表达式表单 */
const permissionExpInfoFormRef = ref();
const permissionExpInfoForm = reactive({
  id: undefined,
  name: undefined,
  expression: undefined,
  desc: undefined,
});
const permissionExpInfoFormRules = {
  name: [{ required: true, message: "限制条件名称未填写" }],
  expression: [{ required: true, message: "SpringEL 表达式未填写" }],
};

/** 权限 */
const permissions = reactive([]);

/**
 * 获取权限表达式详情
 *
 * @param id 权限表达式ID
 */
const handleGetPermissionExpDetail = (id: string) => {
  getPermissionExpDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissionExpId.value = data.id;
        permissionExpName.value = data.name;

        permissionExpInfoForm.id = data.id;
        permissionExpInfoForm.name = data.name;
        permissionExpInfoForm.expression = data.expression;
        permissionExpInfoForm.desc = data.desc;

        permissions.length = 0;
        permissions.push(...data.permissions);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限表达式详情");
    });
};

/**
 * 提交权限表达式表单
 */
const handlePermissionExpInfoFormSubmit = (formData: any) => {
  updatePermissionExp(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetPermissionExpDetail(permissionExpId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新权限表达式");
    });
};

/**
 * 重置权限表达式表单
 */
const handleResetPermissionExpInfoForm = () => {
  permissionExpInfoFormRef.value.resetFields();
  handleGetPermissionExpDetail(permissionExpId.value);
};

/**
 * 删除授权条件
 *
 * @param authorizeId 授权ID
 */
const handleRemoveAuthorizeCondition = (authorizeId: string) => {
  Modal.warning({
    title: "确定取消限制吗？",
    content: "此操作将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      removeAuthorizeCondition({
        authorizeIds: [authorizeId],
        permissionExpIds: [permissionExpId.value],
      })
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("取消限制成功");
            handleGetPermissionExpDetail(permissionExpId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除授权条件");
        });
    },
  });
};

/**
 * 跳转被授权主体详情
 */
const handeToPrincipalDetail = (principal: any) => {
  if (principal.principalType === "USER") {
    handleToUserDetail(principal.principalId);
  }

  if (principal.principalType === "USER_GROUP") {
    hantoToUserGroupDetail(principal.principalId);
  }

  if (principal.principalType === "ROLE") {
    handleToRoleDetail(principal.principalId);
  }
};

/**
 * 跳转用户组详情
 */
const hantoToUserGroupDetail = (id: string) => {
  router.push({
    path: "/user/group/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转用户详情
 */
const handleToUserDetail = (id: string) => {
  router.push({
    path: "/user/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转角色详情
 */
const handleToRoleDetail = (id: string) => {
  router.push({
    path: "/role/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转资源组详情
 */
const handleToResourceGroupDetail = (id: string) => {
  router.push({
    path: "/resource/group/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转资源详情
 */
const handleToResourceDetail = (id: string) => {
  router.push({
    path: "/permission/resource/detail",
    query: {
      id,
    },
  });
};

/**
 * 跳转权限详情
 */
const handleToPermissionDetail = (id: string) => {
  router.push({
    path: "/permission/detail",
    query: {
      id,
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
      const permissionExpId = route.query.id as string;
      handleGetPermissionExpDetail(permissionExpId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      permissionExpId,
      permissionExpName,
      permissionExpInfoFormRef,
      permissionExpInfoForm,
      permissionExpInfoFormRules,
      permissions,
      handeToPrincipalDetail,
      handleToResourceGroupDetail,
      handleToResourceDetail,
      handleToPermissionDetail,
      handleRemoveAuthorizeCondition,
      handlePermissionExpInfoFormSubmit,
      handleResetPermissionExpInfoForm,
    };
  },
});
