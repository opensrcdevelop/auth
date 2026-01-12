import { deletePermissionExp, getPermissionExpList } from "@/api/permission";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";
/** 权限表达式列表 */
var permissionExpList = reactive([]);
var permissionExpSearchKeyword = ref(null);
var permissionExpListPagination;
/**
 * 获取权限表达式列表
 *
 * @param page 页数
 * @param size 条数
 */
var handleGetPermissionExpList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getPermissionExpList({
        page: page,
        size: size,
        keyword: permissionExpSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            permissionExpList.length = 0;
            permissionExpList.push.apply(permissionExpList, data.list);
            permissionExpListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取权限表达式列表");
    });
};
/**
 * 跳转权限表达式详情
 *
 * @param resource 权限表达式
 */
var handleToPermissionExpDetail = function (permissionExp) {
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
var handleDeletePermissionExp = function (permissionExp) {
    Modal.confirm({
        title: "\u786E\u5B9A\u5220\u9664\u9650\u5236\u6761\u4EF6\u300C".concat(permissionExp.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除其关联的全部权限，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deletePermissionExp(permissionExp.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetPermissionExpList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除限制条件");
            });
        },
    });
};
/**
 * 跳转创建限制条件
 */
var handleToCreatePermssionExp = function () {
    router.push({
        path: "/permission/expression/create",
    });
};
export default defineComponent({
    setup: function () {
        permissionExpListPagination = usePagination("permissionExpList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetPermissionExpList(page, size);
        });
        return {
            permissionExpList: permissionExpList,
            permissionExpListPagination: permissionExpListPagination,
            permissionExpSearchKeyword: permissionExpSearchKeyword,
            handleGetPermissionExpList: handleGetPermissionExpList,
            handleToPermissionExpDetail: handleToPermissionExpDetail,
            handleDeletePermissionExp: handleDeletePermissionExp,
            handleToCreatePermssionExp: handleToCreatePermssionExp,
        };
    },
});
