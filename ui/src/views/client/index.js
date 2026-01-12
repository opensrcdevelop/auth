import { deleteClient, getClientList } from "@/api/client";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { Modal, Notification } from "@arco-design/web-vue";
var clientList = reactive([]);
var searchKeyword = ref("");
/**
 * 获取客户端列表
 */
var handleGetClientList = function (keyword, page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = -1; }
    clientList.length = 0;
    var query = {
        keyword: keyword,
        page: page,
        size: size,
    };
    // 去除无效的关键字
    if (keyword.trim().length === 0) {
        delete query.keyword;
    }
    getClientList(query)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            clientList.push.apply(clientList, data.list);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取客户端列表");
    });
};
/**
 * 跳转至客户端详情页
 *
 * @param id
 */
var toClientDetail = function (id) {
    router.push({
        path: "/client/detail",
        query: {
            id: id,
        },
    });
};
/**
 * 跳转至创建客户端页
 */
var toCreateClient = function () {
    router.push({
        path: "/client/create",
    });
};
/**
 * 删除客户端
 *
 * @param client 客户端
 */
var handleDeleteClient = function (client) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u5BA2\u6237\u7AEF\u300C".concat(client.name, "\u300D\u5417\uFF1F"),
        content: "删除后将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deleteClient(client.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetClientList("");
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除客户端");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            handleGetClientList("");
        });
        return {
            clientList: clientList,
            searchKeyword: searchKeyword,
            handleGetClientList: handleGetClientList,
            toClientDetail: toClientDetail,
            toCreateClient: toCreateClient,
            handleDeleteClient: handleDeleteClient,
        };
    },
});
