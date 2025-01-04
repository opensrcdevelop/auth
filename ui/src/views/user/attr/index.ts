import { deleteUserAttr, getUserAttrs } from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";

/** 用户字段列表 */
const userColumnList = reactive([]);
const userColumnSearchKeyword = ref("");
let userColumnListPagination;

/**
 * 获取用户字段列表
 *
 * @param page 页数
 * @param size 大小
 */
const handleGetUserColumnList = (page: number = 1, size: number = 15) => {
  getUserAttrs({
    page,
    size,
    keyword: userColumnSearchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userColumnList.length = 0;
        userColumnList.push(...data.list);

        userColumnListPagination.updatePagination(
          data.current,
          data.total,
          data.size
        );
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户字段列表");
    });
};

/**
 * 跳转至创建用户字段
 */
const handleToCreateUserColumn = () => {
  router.push({
    path: "/user/attr/create",
  });
};

/**
 * 跳转至用户字段详情
 */
const handleToUserColumnDetail = (userColumn: any) => {
  router.push({
    path: "/user/attr/detail",
    query: {
      id: userColumn.id,
      active_tab: "user_column_info",
    },
  });
};

/**
 * 删除用户字段
 */
const handleDeleteUserColumn = (userColumn: any) => {
  Modal.confirm({
    title: `确定删除用户字段「${userColumn.name}」吗？`,
    content: "此操作将不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteUserAttr(userColumn.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetUserColumnList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除用户属性");
        });
    },
  });
};

export default defineComponent({
  setup() {
    userColumnListPagination = usePagination(
      "userColumnList",
      ({ page, size }) => {
        handleGetUserColumnList(page, size);
      }
    );

    return {
      userColumnList,
      userColumnSearchKeyword,
      userColumnListPagination,
      handleGetUserColumnList,
      handleToCreateUserColumn,
      handleToUserColumnDetail,
      handleDeleteUserColumn,
    };
  },
});
