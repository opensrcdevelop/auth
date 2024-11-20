import { deleteTenant, getTenantList } from "@/api/tenant";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";

/** 租户列表 */
const tenantList = reactive([]);
const tenantSerachKeyword = ref(null);
const tenantPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

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

        tenantPagination.current = data.current;
        tenantPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取租户列表");
    });
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  tenantPagination.current = page;
  handleGetTenantList(page, tenantPagination.pageSize);
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  tenantPagination.pageSize = size;
  handleGetTenantList(1, size);
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
    onMounted(() => {
      handleGetTenantList();
    });

    return {
      tenantList,
      tenantSerachKeyword,
      tenantPagination,
      handleGetTenantList,
      handlePageChange,
      handlePageSizeChange,
      handleToTenantDetail,
      handleToCreateTenant,
      handleDeleteTenant
    };
  },
});
