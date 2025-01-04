import { deleteRole, getRoleList } from "@/api/role";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";

const roleList = reactive([]);
const searchKeyword = ref("");
let roleListPagination;

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

        roleListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取角色列表");
    });
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
      active_tab: "role_info",
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
    roleListPagination = usePagination("roleList", ({ page, size }) => {
      handleGetRoleList(page, size);
    });

    return {
      searchKeyword,
      roleList,
      handleGetRoleList,
      handleToRoleDetail,
      handleToCreateRole,
      handleDeleteRole,
      roleListPagination,
    };
  },
});
