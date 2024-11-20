import { deletePermissionExp, getPermissionExpList } from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";

/** 权限表达式列表 */
const permissionExpList = reactive([]);
const permissionExpSearchKeyword = ref(null);
const permissionExpListPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取权限表达式列表
 *
 * @param page 页数
 * @param size 条数
 */
const handleGetPermissionExpList = (page: number = 1, size: number = 15) => {
  getPermissionExpList({
    page,
    size,
    keyword: permissionExpSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        permissionExpList.length = 0;
        permissionExpList.push(...data.list);

        permissionExpListPagination.current = data.current;
        permissionExpListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限表达式列表");
    });
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  permissionExpListPagination.current = page;
  handleGetPermissionExpList(page, permissionExpListPagination.pageSize);
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  permissionExpListPagination.pageSize = size;
  handleGetPermissionExpList(1, size);
};

/**
 * 跳转权限表达式详情
 *
 * @param resource 权限表达式
 */
const handleToPermissionExpDetail = (permissionExp: any) => {
  router.push({
    path: "/permission/expression/detail",
    query: {
      id: permissionExp.id,
    },
  });
};

/**
 * 删除限制条件
 *
 * @param permissionExp 限制条件
 */
const handleDeletePermissionExp = (permissionExp: any) => {
  Modal.confirm({
    title: `确定删除限制条件「${permissionExp.name}」吗？`,
    content: "此操作将删除其关联的全部权限，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deletePermissionExp(permissionExp.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetPermissionExpList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除限制条件");
        });
    },
  });
};

/**
 * 跳转创建限制条件
 */
const handleToCreatePermssionExp = () => {
  router.push({
    path: "/permission/expression/create",
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetPermissionExpList();
    });

    return {
      permissionExpList,
      permissionExpListPagination,
      permissionExpSearchKeyword,
      handleGetPermissionExpList,
      handlePageChange,
      handlePageSizeChange,
      handleToPermissionExpDetail,
      handleDeletePermissionExp,
      handleToCreatePermssionExp,
    };
  },
});
