import { deleteIdentitySource, getIdentitySourceList, updateIdentitySource, } from "@/api/identitySource";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, reactive, ref } from "vue";
/** 身份源列表 */
var identitySourceList = reactive([]);
var searchKeyword = ref(null);
var identitySourceListPagination;
/**
 * 获取身份源列表
 */
var handleGetIdentitySourceList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getIdentitySourceList({
        page: page,
        size: size,
        keyword: searchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            identitySourceList.length = 0;
            identitySourceList.push.apply(identitySourceList, data.list);
            identitySourceListPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取身份源列表");
    });
};
/**
 * 跳转到身份源详情
 */
var handleToIdentitySourceDetail = function (identitySource) {
    router.push({
        path: "/identitySource/detail",
        query: {
            id: identitySource.id,
            active_tab: "registration_info",
        },
    });
};
/**
 * 跳转到身份源提供商详情
 */
var handleToProviderDetail = function (identitySource) {
    router.push({
        path: "/identitySource/provider/detail",
        query: {
            id: identitySource.provider.id,
            active_tab: "provider_info",
        },
    });
};
/**
 * 更新身份源状态
 */
var handleUpdateIdentitySourceState = function (identitySource) {
    updateIdentitySource({
        id: identitySource.id,
        enabled: identitySource.enabled,
    })
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("更新身份源状态成功");
            handleGetIdentitySourceList();
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新身份源状态");
    });
};
/**
 * 删除身份源
 */
var handleDeleteIdentitySource = function (identitySource) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u8EAB\u4EFD\u6E90\u300C".concat(identitySource.name, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该身份源及与用户的绑定关系，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteIdentitySource(identitySource.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetIdentitySourceList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除身份源");
            });
        },
    });
};
/**
 * 跳转到创建身份源
 */
var handleToCreateIdentitySource = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.identitySourceProvider.id = undefined;
    globalVariables.identitySourceProvider.name = undefined;
    globalVariables.saveData();
    router.push({
        path: "/identitySource/create",
    });
};
export default defineComponent({
    setup: function () {
        identitySourceListPagination = usePagination("identitySourceList", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetIdentitySourceList(page, size);
        });
        return {
            identitySourceList: identitySourceList,
            searchKeyword: searchKeyword,
            identitySourceListPagination: identitySourceListPagination,
            handleGetIdentitySourceList: handleGetIdentitySourceList,
            handleToIdentitySourceDetail: handleToIdentitySourceDetail,
            handleToProviderDetail: handleToProviderDetail,
            handleUpdateIdentitySourceState: handleUpdateIdentitySourceState,
            handleDeleteIdentitySource: handleDeleteIdentitySource,
            handleToCreateIdentitySource: handleToCreateIdentitySource,
        };
    },
});
