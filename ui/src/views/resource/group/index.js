import { deleteResourceGroup, getResourceGroupList } from "@/api/resourceGroup";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
import { usePagination } from "@/hooks/usePagination";
/** 资源列表 */
var resourceGroupList = reactive([]);
var resourceGroupSearchKeyword = ref(null);
var resourceGroupListPagination;
/**
 * 获取资源组列表
 *
 * @param page 页数
 * @param size 条数
 */
var handleGetResourceGroupList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getResourceGroupList({
        page: page,
        size: size,
        keyword: resourceGroupSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            resourceGroupList.length = 0;
            resourceGroupList.push.apply(resourceGroupList, data.list);
            resourceGroupListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取资源列表");
    });
};
/**
 * 跳转到资源组详情
 */
var handleToResourceGroupDetail = function (resourceGroup) {
    router.push({
        path: "/resource/group/detail",
        query: {
            id: resourceGroup.id,
            active_tab: "resource_group_info",
        },
    });
};
/**
 * 删除资源组
 */
var handleDeleteResourceGroup = function (resourceGroup) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u8D44\u6E90\u7EC4\u300C".concat(resourceGroup.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该资源组内的所有资源，资源关联的所有权限及权限授权，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteResourceGroup(resourceGroup.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetResourceGroupList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除资源组");
            });
        },
    });
};
/**
 * 跳转资源组
 */
var handleToCreateResourceGroup = function () {
    router.push({
        path: "/resource/group/create",
    });
};
export default defineComponent({
    setup: function () {
        resourceGroupListPagination = usePagination("resourceGroupList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetResourceGroupList(page, size);
        });
        onMounted(function () {
            handleGetResourceGroupList(resourceGroupListPagination.pagination.current, resourceGroupListPagination.pagination.pageSize);
        });
        return {
            resourceGroupList: resourceGroupList,
            resourceGroupSearchKeyword: resourceGroupSearchKeyword,
            resourceGroupListPagination: resourceGroupListPagination,
            handleGetResourceGroupList: handleGetResourceGroupList,
            handleToResourceGroupDetail: handleToResourceGroupDetail,
            handleDeleteResourceGroup: handleDeleteResourceGroup,
            handleToCreateResourceGroup: handleToCreateResourceGroup,
        };
    },
});
