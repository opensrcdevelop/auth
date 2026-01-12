import { deleteUserAttr, getUserAttrs } from "@/api/user";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";
/** 用户字段列表 */
var userColumnList = reactive([]);
var userColumnSearchKeyword = ref("");
var userColumnListPagination;
/**
 * 获取用户字段列表
 *
 * @param page 页数
 * @param size 大小
 */
var handleGetUserColumnList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getUserAttrs({
        page: page,
        size: size,
        keyword: userColumnSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userColumnList.length = 0;
            userColumnList.push.apply(userColumnList, data.list);
            userColumnListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户字段列表");
    });
};
/**
 * 跳转至创建用户字段
 */
var handleToCreateUserColumn = function () {
    router.push({
        path: "/user/attr/create",
    });
};
/**
 * 跳转至用户字段详情
 */
var handleToUserColumnDetail = function (userColumn) {
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
var handleDeleteUserColumn = function (userColumn) {
    Modal.confirm({
        title: "\u786E\u5B9A\u5220\u9664\u7528\u6237\u5B57\u6BB5\u300C".concat(userColumn.name, "\u300D\u5417\uFF1F"),
        content: "此操作将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteUserAttr(userColumn.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetUserColumnList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除用户属性");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        userColumnListPagination = usePagination("userColumnList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetUserColumnList(page, size);
        });
        return {
            userColumnList: userColumnList,
            userColumnSearchKeyword: userColumnSearchKeyword,
            userColumnListPagination: userColumnListPagination,
            handleGetUserColumnList: handleGetUserColumnList,
            handleToCreateUserColumn: handleToCreateUserColumn,
            handleToUserColumnDetail: handleToUserColumnDetail,
            handleDeleteUserColumn: handleDeleteUserColumn,
        };
    },
});
