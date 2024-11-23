import { deleteRole, getRoleList } from "@/api/role";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";

const roleList = reactive([]);
const searchKeyword = ref("");
const roleListPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取角色列表
 */
const handleGetRoleList = (page: number = 1, size: number = 15) => {
  getRoleList({
    page,
    size,
    keyword: searchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data) => {
        roleList.length = 0;
        roleList.push(...data.list);

        roleListPagination.current = data.current;
        roleListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取角色列表");
    });
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  roleListPagination.current = page;
  handleGetRoleList(page, roleListPagination.pageSize);
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  roleListPagination.pageSize = size;
  handleGetRoleList(1, size);
};

/**
 * 跳转角色详情
 *
 * @param role 角色
 */
const handleToRoleDetail = (role: any) => {
  router.push({
    path: "/role/detail",
    query: {
      id: role.id,
    },
  });
};

/**
 * 跳转创建角色
 */
const handleToCreateRole = () => {
  router.push({
    path: "/role/create",
  });
};

/**
 * 删除角色
 */
const handleDeleteRole = (role: any) => {
  Modal.warning({
    title: `确定删除角色「${role.name}」吗？`,
    content: "此操作将删除该角色关联的用户（组）及授权，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteRole(role.id)
        .then((result: any) => {
          Notification.success("删除成功");
          handleGetRoleList(null);
        })
        .catch((err: any) => {
          handleApiError(err, "删除角色");
        });
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetRoleList(null);
    });

    return {
      searchKeyword,
      roleList,
      handleGetRoleList,
      handleToRoleDetail,
      handleToCreateRole,
      handleDeleteRole,
      roleListPagination,
      handlePageChange,
      handlePageSizeChange,
    };
  },
});
