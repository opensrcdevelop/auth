import { deleteTenant, getTenantList } from "@/api/tenant";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";

/** 租户列表 */
const tenantList = reactive([]);
const tenantSerachKeyword = ref(null);
let tenantPagination;

/**
 * 获取租户列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetTenantList = (page: number = 1, size: number = 15) => {
  getTenantList({
    page,
    size,
    keyword: tenantSerachKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        tenantList.length = 0;
        tenantList.push(...data.list);

        tenantPagination.updatePagination(data.current, data.total, data.size);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取租户列表");
    });
};

/**
 * 跳转租户详情
 *
 * @param tenant 租户
 */
const handleToTenantDetail = (tenant: any) => {
  router.push({
    path: "/tenant/detail",
    query: {
      id: tenant.id,
      active_tab: "tanant_info",
    },
  });
};

/**
 * 跳转创建租户
 */
const handleToCreateTenant = () => {
  router.push({
    path: "/tenant/create",
  });
};

/**
 * 删除租户
 *
 * @param tenant 租户
 */
const handleDeleteTenant = (tenant: any) => {
  Modal.confirm({
    title: `确定删除租户「${tenant.name}」吗？`,
    content: "此操作将删除其关联的全部数据，将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteTenant(tenant.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetTenantList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除租户");
        });
    },
  });
};

export default defineComponent({
  setup() {
    tenantPagination = usePagination("tenantList", ({ page, size }) => {
      handleGetTenantList(page, size);
    });

    return {
      tenantList,
      tenantSerachKeyword,
      tenantPagination,
      handleGetTenantList,
      handleToTenantDetail,
      handleToCreateTenant,
      handleDeleteTenant,
    };
  },
});
