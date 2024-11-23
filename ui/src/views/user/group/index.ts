import { deleteUserGroup, getUserGroupList } from "@/api/userGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";

const userGroupList = reactive([]);
const searchKeyword = ref("");
const userGroupListPagination = reactive({
  total: 0,
  current: 1,
  pageSize: 15,
  showPageSize: true,
  showTotal: true,
  pageSizeOptions: [15, 25, 50],
});

/**
 * 获取用户组列表
 */
const handleGetUserGroupList = (page: number = 1, size: number = 15) => {
  getUserGroupList({
    page,
    size,
    keyword: searchKeyword.value,
  })
    .then((result: any) => {
      handleApiSuccess(result, (data: any) => {
        userGroupList.length = 0;
        userGroupList.push(...data.list);

        userGroupListPagination.current = data.current;
        userGroupListPagination.total = data.total;
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户组列表");
    });
};

/**
 * 页数变化
 */
const handlePageChange = (page: number) => {
  userGroupListPagination.current = page;
  handleGetUserGroupList(page, userGroupListPagination.pageSize);
};

/**
 * 分页大小变化
 */
const handlePageSizeChange = (size: number) => {
  userGroupListPagination.pageSize = size;
  handleGetUserGroupList(1, size);
};


/**
 * 跳转用户组详情
 *
 * @param userGroup 用户组
 */
const hantoToUserGroupDetail = (userGroup: any) => {
  router.push({
    path: "/user/group/detail",
    query: {
      id: userGroup.id,
    },
  });
};

/**
 * 跳转创建用户组
 */
const handleToCreateUserGroup = () => {
  router.push("/user/group/create");
};

/**
 * 删除用户组
 *
 * @param userGroup 用户组
 */
const handleDeleteUserGroup = (userGroup: any) => {
  Modal.warning({
    title: `确定删除用户组「${userGroup.name}」吗？`,
    content: "此操作不可恢复，请谨慎操作。",
    hideCancel: false,
    okButtonProps: {
      status: "danger",
    },
    onOk: () => {
      deleteUserGroup(userGroup.id)
        .then((result: any) => {
          handleApiSuccess(result, () => {
            Notification.success("删除成功");
            handleGetUserGroupList();
          });
        })
        .catch((err: any) => {
          handleApiError(err, "删除用户组");
        });
    },
  });
};

export default defineComponent({
  setup() {
    onMounted(() => {
      handleGetUserGroupList(null);
    });

    return {
      userGroupList,
      handleGetUserGroupList,
      searchKeyword,
      hantoToUserGroupDetail,
      handleToCreateUserGroup,
      handleDeleteUserGroup,
      userGroupListPagination,
      handlePageChange,
      handlePageSizeChange,
    };
  },
});
