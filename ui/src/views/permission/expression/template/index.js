import { deletePermissionExpTemplate, getPermissionExpTemplateList, } from "@/api/permission";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { defineComponent, onMounted, reactive, ref } from "vue";
/**
 * 跳转创建模板
 */
var handleToCreateTemplate = function () {
    router.push({
        path: "/permission/expression/template/create",
    });
};
/**
 * 跳转模板详情
 */
var handleToTemplateDetail = function (id) {
    router.push({
        path: "/permission/expression/template/detail",
        query: {
            id: id,
        },
    });
};
/**
 * 模板列表
 */
var templateList = reactive([]);
/**
 * 模板名称搜索关键字
 */
var searchKeyword = ref(undefined);
/**
 * 获取模板列表
 */
var handleGetTemplateList = function () {
    getPermissionExpTemplateList({
        keyword: searchKeyword.value,
        page: 1,
        size: -1,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            templateList.length = 0;
            templateList.push.apply(templateList, data.list);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取权限表达式模板列表");
    });
};
/**
 * 删除模板
 */
var handleDeleteTemplate = function (template) {
    Modal.confirm({
        title: "\u786E\u5B9A\u5220\u9664\u6A21\u677F\u300C".concat(template.name, "\u300D\u53CA\u5173\u8054\u7684\u6743\u9650\u8868\u8FBE\u5F0F\u5417\uFF1F"),
        content: "此操作将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deletePermissionExpTemplate(template.id)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetTemplateList();
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除权限表达式模板");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            handleGetTemplateList();
        });
        return {
            handleToCreateTemplate: handleToCreateTemplate,
            searchKeyword: searchKeyword,
            templateList: templateList,
            handleGetTemplateList: handleGetTemplateList,
            handleToTemplateDetail: handleToTemplateDetail,
            handleDeleteTemplate: handleDeleteTemplate,
        };
    },
});
