import { deleteTenant, getTenantList } from "@/api/tenant";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, reactive, ref } from "vue";
/** 租户列表 */
var tenantList = reactive([]);
var tenantSerachKeyword = ref(null);
var tenantPagination;
/**
 * 获取租户列表
 *
 * @param page 页数
 * @param size 条数
 */
var handleGetTenantList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getTenantList({
        page: page,
        size: size,
        keyword: tenantSerachKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            tenantList.length = 0;
            tenantList.push.apply(tenantList, data.list);
            tenantPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取租户列表");
    });
};
/**
 * 跳转租户详情
 *
 * @param tenant 租户
 */
var handleToTenantDetail = function (tenant) {
    router.push({
        path: "/tenant/detail",
        query: {
            id: tenant.id,
            active_tab: "tanant_info",
        },
    });
};
/**
 * 跳转创建租户
 */
var handleToCreateTenant = function () {
    router.push({
        path: "/tenant/create",
    });
};
/**
 * 删除租户
 *
 * @param tenant 租户
 */
var handleDeleteTenant = function (tenant) {
    Modal.confirm({
        title: "\u786E\u5B9A\u5220\u9664\u79DF\u6237\u300C".concat(tenant.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除其关联的全部数据，将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteTenant(tenant.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetTenantList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除租户");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        tenantPagination = usePagination("tenantList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetTenantList(page, size);
        });
        return {
            tenantList: tenantList,
            tenantSerachKeyword: tenantSerachKeyword,
            tenantPagination: tenantPagination,
            handleGetTenantList: handleGetTenantList,
            handleToTenantDetail: handleToTenantDetail,
            handleToCreateTenant: handleToCreateTenant,
            handleDeleteTenant: handleDeleteTenant,
        };
    },
});
