import { deleteUserGroup, getUserGroupList } from "@/api/userGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";
var userGroupList = reactive([]);
var searchKeyword = ref("");
var userGroupListPagination;
/**
 * 获取用户组列表
 */
var handleGetUserGroupList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserGroupList({
        page: page,
        size: size,
        keyword: searchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userGroupList.length = 0;
            userGroupList.push.apply(userGroupList, data.list);
            userGroupListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户组列表");
    });
};
/**
 * 跳转用户组详情
 *
 * @param userGroup 用户组
 */
var hantoToUserGroupDetail = function (userGroup) {
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
var handleToCreateUserGroup = function () {
    router.push("/user/group/create");
};
/**
 * 删除用户组
 *
 * @param userGroup 用户组
 */
var handleDeleteUserGroup = function (userGroup) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u7528\u6237\u7EC4\u300C".concat(userGroup.name, "\u300D\u5417\uFF1F"),
        content: "此操作不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteUserGroup(userGroup.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetUserGroupList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除用户组");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        userGroupListPagination = usePagination("userGroupList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetUserGroupList(page, size);
        });
        return {
            userGroupList: userGroupList,
            handleGetUserGroupList: handleGetUserGroupList,
            searchKeyword: searchKeyword,
            hantoToUserGroupDetail: hantoToUserGroupDetail,
            handleToCreateUserGroup: handleToCreateUserGroup,
            handleDeleteUserGroup: handleDeleteUserGroup,
            userGroupListPagination: userGroupListPagination,
        };
    },
});
