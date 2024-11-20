import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { useRoute } from "vue-router";
import {
  getResourceDetail,
  getResourcePermissions,
  updateResource,
} from "@/api/resource";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { deletePermission } from "@/api/permission";

/**
 * 返回上一级
 */
const handleBack = () => {
  router.back();
};

const activeTab = ref("resource_info");

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

const resourceId = ref("");
const resourceName = ref("");

/** 资源组信息 */
const resourceGroupInfo = reactive({
  id: undefined,
  name: undefined,
  code: undefined,
});

/** 资源基本信息 */
const resourceInfoFormRef = ref();
const resourceInfoForm = reactive({
  id: undefined,
  name: undefined,
  code: undefined,
  desc: undefined,
  api: undefined,
});
const resourceInfoFormRuels = {
  name: [{ required: true, message: "资源名称未填写" }],
  code: [
    { required: true, message: "资源标识未填写" },
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
 * 获取资源详情
 *
 * @param id 资源ID
 */
const handleGetResourceDetail = (id: string) => {
  getResourceDetail(id)
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        resourceId.value = data.id;
        resourceName.value = data.name;

        resourceGroupInfo.id = data.resourceGroup.id;
        resourceGroupInfo.name = data.resourceGroup.name;
        resourceGroupInfo.code = data.resourceGroup.code;

        resourceInfoForm.id = data.id;
        resourceInfoForm.name = data.name;
        resourceInfoForm.code = data.code;
        resourceInfoForm.desc = data.desc;
        resourceInfoForm.api = data.api;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取资源详情");
    });
};

/**
 * 资源基本信息表单提交
 */
const handleResourceInfoFormSubmit = (formData: any) => {
  updateResource(formData)
    .then((result: any) => {
      handleApiSuccess(result, () => {
        Notification.success("保存成功");
        handleGetResourceDetail(resourceId.value);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "更新资源");
    });
};

/**
 * 重置资源基本信息表单
 */
const handleResetResourceInfoForm = () => {
  resourceInfoFormRef.value.resetFields();
  handleGetResourceDetail(resourceId.value);
};

/** 权限信息  */
const permissions = reactive([]);
const permissionSearchKeyword = ref(null);
const permissionsPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取资源内权限
 *
 * @param id 资源ID
 * @param page 页数
 * @param size 条数
 */
const handleGetResourcePermissions = (
  id: string,
  page: number = 1,
  size: number = 15
) => {
  getResourcePermissions(id, {
    page,
    size,
    keyword: permissionSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissions.length = 0;
        permissions.push(...data.list);

        permissionsPagination.total = data.total;
        permissionsPagination.current = data.current;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取资源内权限");
    });
};

/**
 * 检索资源内权限
 */
const handleSearchResourcePermissions = () => {
  handleGetResourcePermissions(
    resourceId.value,
    1,
    permissionsPagination.pageSize
  );
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  permissionsPagination.current = page;
  handleGetResourcePermissions(
    resourceId.value,
    page,
    permissionsPagination.pageSize
  );
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  permissionsPagination.pageSize = size;
  handleGetResourcePermissions(resourceId.value, 1, size);
};

/**
 * 跳转到权限详情
 */
const handleToPermissionDetail = (permission: any) => {
  router.push({
    path: "/permission/detail",
    query: {
      id: permission.permissionId,
    },
  });
};

/**
 * 跳转创建权限
 */
const handleToCreatePermission = () => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.resourceId = resourceId.value;
  router.push({
    path: "/permission/create",
  });
};

/**
 * 删除权限
 */
const handleDeletePermission = (permission: any) => {
  Modal.warning({
    title: `确定删除权限「${permission.permissionName}」吗？`,
    content: "此操作将删除该权限关联的授权，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deletePermission(permission.permissionId)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetResourcePermissions(resourceId.value);
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除权限");
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
      const resourceId = route.query.id as string;
      handleGetResourceDetail(resourceId);
      handleGetResourcePermissions(resourceId);
    });

    return {
      handleBack,
      activeTab,
      handleTabChange,
      resourceId,
      resourceName,
      resourceInfoFormRef,
      resourceInfoForm,
      resourceInfoFormRuels,
      resourceGroupInfo,
      permissions,
      permissionsPagination,
      permissionSearchKeyword,
      handleSearchResourcePermissions,
      handlePageChange,
      handlePageSizeChange,
      handleToPermissionDetail,
      handleResourceInfoFormSubmit,
      handleResetResourceInfoForm,
      handleToCreatePermission,
      handleDeletePermission
    };
  },
});
