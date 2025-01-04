import { deletePermissionExp, getPermissionExpList } from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { usePagination } from "@/hooks/usePagination";

/** 权限表达式列表 */
const permissionExpList = reactive([]);
const permissionExpSearchKeyword = ref(null);
let permissionExpListPagination;

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

        permissionExpListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取权限表达式列表");
    });
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
      active_tab: "condition_info",
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

/**
 * 跳转调试限制条件
 */
const handleToDebugPermissionExp = (permissionExp: any) => {
  const globalVariables = useGlobalVariablesStore();
  globalVariables.permissionExp = permissionExp;
  globalVariables.saveData();
  router.push({
    path: "/permission/expression/debug",
  });
};

export default defineComponent({
  setup() {
    permissionExpListPagination = usePagination(
      "permissionExpList",
      ({ page, size }) => {
        handleGetPermissionExpList(page, size);
      }
    );

    return {
      permissionExpList,
      permissionExpListPagination,
      permissionExpSearchKeyword,
      handleGetPermissionExpList,
      handleToPermissionExpDetail,
      handleDeletePermissionExp,
      handleToCreatePermssionExp,
      handleToDebugPermissionExp,
    };
  },
});
