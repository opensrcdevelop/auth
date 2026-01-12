import { deleteRole, getRoleList } from "@/api/role";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";
var roleList = reactive([]);
var searchKeyword = ref("");
var roleListPagination;
/**
 * 获取角色列表
 */
var handleGetRoleList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getRoleList({
        page: page,
        size: size,
        keyword: searchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            roleList.length = 0;
            roleList.push.apply(roleList, data.list);
            roleListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取角色列表");
    });
};
/**
 * 跳转角色详情
 *
 * @param role 角色
 */
var handleToRoleDetail = function (role) {
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
var handleToCreateRole = function () {
    router.push({
        path: "/role/create",
    });
};
/**
 * 删除角色
 */
var handleDeleteRole = function (role) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u89D2\u8272\u300C".concat(role.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该角色关联的用户（组）及授权，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteRole(role.id)
                .then(function (result) {
                Notification.success("删除成功");
                handleGetRoleList(null);
            })
                .catch(function (err) {
                handleApiError(err, "删除角色");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        roleListPagination = usePagination("roleList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetRoleList(page, size);
        });
        return {
            searchKeyword: searchKeyword,
            roleList: roleList,
            handleGetRoleList: handleGetRoleList,
            handleToRoleDetail: handleToRoleDetail,
            handleToCreateRole: handleToCreateRole,
            handleDeleteRole: handleDeleteRole,
            roleListPagination: roleListPagination,
        };
    },
});
