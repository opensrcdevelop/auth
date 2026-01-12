import { deleteResource, getResourceList } from "@/api/resource";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { usePagination } from "@/hooks/usePagination";
/** 资源列表 */
var resourceList = reactive([]);
var resourceSearchKeyword = ref(null);
var resourceListPagination;
/**
 * 获取资源列表
 *
 * @param page 页数
 * @param size 条数
 */
var handleGetResourceList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getResourceList({
        page: page,
        size: size,
        keyword: resourceSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            resourceList.length = 0;
            resourceList.push.apply(resourceList, data.list);
            resourceListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取资源列表");
    });
};
/**
 * 跳转资源详情
 *
 * @param resource 资源
 */
var handleToResourceDetail = function (resource) {
    router.push({
        path: "/permission/resource/detail",
        query: {
            id: resource.id,
            active_tab: "resource_info",
        },
    });
};
/**
 * 跳转创建资源
 */
var handleToCreateResource = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.resourceGroupId = undefined;
    globalVariables.saveData();
    router.push({
        path: "/permission/resource/create",
    });
};
/**
 * 删除资源
 */
var handleDeleteResource = function (resource) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u8D44\u6E90\u300C".concat(resource.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该资源关联的权限及权限关联的授权，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteResource(resource.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetResourceList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除资源");
            });
        },
    });
};
/** 授权对话框 */
var authorizeVisible = ref(false);
/**
 * 授权
 */
var handleAuthorize = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.authorizeOptions.principal = undefined;
    globalVariables.authorizeOptions.principalId = undefined;
    globalVariables.authorizeOptions.principalType = undefined;
    authorizeVisible.value = true;
};
export default defineComponent({
    setup: function () {
        resourceListPagination = usePagination("resourceList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetResourceList(page, size);
        });
        return {
            resourceList: resourceList,
            resourceSearchKeyword: resourceSearchKeyword,
            resourceListPagination: resourceListPagination,
            handleGetResourceList: handleGetResourceList,
            handleToResourceDetail: handleToResourceDetail,
            handleToCreateResource: handleToCreateResource,
            handleDeleteResource: handleDeleteResource,
            authorizeVisible: authorizeVisible,
            handleAuthorize: handleAuthorize,
        };
    },
});
