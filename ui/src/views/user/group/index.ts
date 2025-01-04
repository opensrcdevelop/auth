import { deleteUserGroup, getUserGroupList } from "@/api/userGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";

const userGroupList = reactive([]);
const searchKeyword = ref("");
let userGroupListPagination;

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

        userGroupListPagination.updatePagination(data.current, data.total, data.size);
      });
    })
    .catch((err: any) => {
      handleApiError(err, "获取用户组列表");
    });
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
      "active_tab": "user_group_info"
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
    userGroupListPagination = usePagination("userGroupList", ({page, size}) => {
      handleGetUserGroupList(page, size);
    });

    return {
      userGroupList,
      handleGetUserGroupList,
      searchKeyword,
      hantoToUserGroupDetail,
      handleToCreateUserGroup,
      handleDeleteUserGroup,
      userGroupListPagination,
    };
  },
});
