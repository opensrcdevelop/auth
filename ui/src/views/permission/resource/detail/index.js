var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
import { defineComponent, onMounted, reactive, ref } from "vue";
import router from "@/router";
import { getResourceDetail, getResourcePermissions, updateResource, } from "@/api/resource";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
import { useGlobalVariablesStore } from "@/store/globalVariables";
import { deletePermission } from "@/api/permission";
import { usePagination } from "@/hooks/usePagination";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("resource_info");
/**
 * tab 切换事件
 *
 * @param tabKey tabKey
 */
var handleTabChange = function (tabKey) {
    router.replace({
        query: __assign(__assign({}, router.currentRoute.value.query), { active_tab: tabKey }),
    });
    activeTab.value = tabKey;
    handleTabInit(tabKey);
};
var handleTabInit = function (tabKey, id) {
    if (id === void 0) { id = resourceId.value; }
    switch (tabKey) {
        case "resource_info":
            handleGetResourceDetail(id);
            break;
        case "permission_list":
            if (!resourceId.value) {
                handleGetResourceDetail(id);
            }
            handleGetResourcePermissions(id);
            break;
    }
};
var resourceId = ref("");
var resourceName = ref("");
/** 资源组信息 */
var resourceGroupInfo = reactive({
    id: undefined,
    name: undefined,
    code: undefined,
});
/** 资源基本信息 */
var resourceInfoFormRef = ref();
var resourceInfoForm = reactive({
    id: undefined,
    name: undefined,
    code: undefined,
    desc: undefined,
    api: undefined,
    resourceGroupId: undefined,
});
var resourceInfoFormRuels = {
    name: [{ required: true, message: "资源名称未填写" }],
    code: [
        { required: true, message: "资源标识未填写" },
        {
            validator: function (value, cb) {
                if (value && !/^[A-Za-z0-9-\_]+$/.test(value)) {
                    cb("只允许包含英文字母、数字、下划线_、横线-");
                }
                else {
                    cb();
                }
            },
        },
    ],
};
/**
 * 获取资源详情
 *
 * @param id 资源ID
 */
var handleGetResourceDetail = function (id) {
    getResourceDetail(id)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            resourceId.value = data.id;
            resourceName.value = data.name;
            resourceGroupInfo.id = data.resourceGroup.id;
            resourceGroupInfo.name = data.resourceGroup.name;
            resourceGroupInfo.code = data.resourceGroup.code;
            resourceInfoForm.id = data.id;
            resourceInfoForm.name = data.name;
            resourceInfoForm.code = data.code;
            resourceInfoForm.desc = data.desc;
            resourceInfoForm.api = data.api;
            resourceInfoForm.resourceGroupId = data.resourceGroup.id;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取资源详情");
    });
};
/**
 * 资源基本信息表单提交
 */
var handleResourceInfoFormSubmit = function (formData) {
    updateResource(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetResourceDetail(resourceId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新资源");
    });
};
/**
 * 重置资源基本信息表单
 */
var handleResetResourceInfoForm = function () {
    resourceInfoFormRef.value.resetFields();
    handleGetResourceDetail(resourceId.value);
};
/** 权限信息  */
var permissions = reactive([]);
var permissionSearchKeyword = ref(null);
var permissionsPagination;
/**
 * 获取资源内权限
 *
 * @param id 资源ID
 * @param page 页数
 * @param size 条数
 */
var handleGetResourcePermissions = function (id, page, size) {
    if (id === void 0) { id = resourceId.value; }
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getResourcePermissions(id, {
        page: page,
        size: size,
        keyword: permissionSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            permissions.length = 0;
            permissions.push.apply(permissions, data.list);
            permissionsPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取资源内权限");
    });
};
/**
 * 检索资源内权限
 */
var handleSearchResourcePermissions = function () {
    handleGetResourcePermissions(resourceId.value, 1, permissionsPagination.pageSize);
};
/**
 * 跳转到权限详情
 */
var handleToPermissionDetail = function (permission) {
    router.push({
        path: "/permission/detail",
        query: {
            id: permission.permissionId,
            active_tab: "permission_info",
        },
    });
};
/**
 * 跳转创建权限
 */
var handleToCreatePermission = function () {
    var globalVariables = useGlobalVariablesStore();
    globalVariables.resourceId = resourceId.value;
    router.push({
        path: "/permission/create",
    });
};
/**
 * 删除权限
 */
var handleDeletePermission = function (permission) {
    Modal.warning({
        title: "\u786E\u5B9A\u5220\u9664\u6743\u9650\u300C".concat(permission.permissionName, "\u300D\u5417\uFF1F"),
        content: "此操作将删除该权限关联的授权，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            deletePermission(permission.permissionId)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("删除成功");
                    handleGetResourcePermissions(resourceId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "删除权限");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        var resourceId = getQueryString("id");
        permissionsPagination = usePagination("".concat(resourceId, "_resourcePermissions"), function (_a) {
            var page = _a.page, size = _a.size;
            if (getQueryString("active_tab") === "permission_list") {
                handleGetResourcePermissions(resourceId, page, size);
            }
        });
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "resource_info";
            handleTabInit(activeTab.value, resourceId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            resourceId: resourceId,
            resourceName: resourceName,
            resourceInfoFormRef: resourceInfoFormRef,
            resourceInfoForm: resourceInfoForm,
            resourceInfoFormRuels: resourceInfoFormRuels,
            resourceGroupInfo: resourceGroupInfo,
            permissions: permissions,
            permissionsPagination: permissionsPagination,
            permissionSearchKeyword: permissionSearchKeyword,
            handleSearchResourcePermissions: handleSearchResourcePermissions,
            handleToPermissionDetail: handleToPermissionDetail,
            handleResourceInfoFormSubmit: handleResourceInfoFormSubmit,
            handleResetResourceInfoForm: handleResetResourceInfoForm,
            handleToCreatePermission: handleToCreatePermission,
            handleDeletePermission: handleDeletePermission,
        };
    },
});
