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
import { addAuthorizeCondition, cancelAuthorization, getPermissionDetail, getPermissionExpList, removeAuthorizeCondition, updateAuthorizePriority, updatePermission, } from "@/api/permission";
import { getQueryString, handleApiError, handleApiSuccess } from "@/util/tool";
import { Modal, Notification } from "@arco-design/web-vue";
/**
 * 返回上一级
 */
var handleBack = function () {
    router.back();
};
var activeTab = ref("permission_info");
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
};
var permissionId = ref("");
var permissionName = ref("");
var permissionLocator = ref("");
/** 权限基本信息 */
var permissionInfoFormRef = ref();
var permissionInfoForm = reactive({
    id: undefined,
    code: undefined,
    name: undefined,
    desc: undefined,
});
var permissionInfoFormRules = {
    name: [{ required: true, message: "权限名称未填写" }],
    code: [
        { required: true, message: "权限编码未填写" },
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
 * 提交权限基本信息表单
 *
 * @param formData 权限基本信息表单
 */
var permissionInfoFormSubmit = function (formData) {
    updatePermission(formData)
        .then(function (result) {
        handleApiSuccess(result, function () {
            Notification.success("保存成功");
            handleGetPermissionDetail(permissionId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "更新权限");
    });
};
/**
 * 重置权限基本信息表单
 */
var handleResetPermissionInfoForm = function () {
    permissionInfoFormRef.value.resetFields();
    handleGetPermissionDetail(permissionId.value);
};
/** 授权记录 */
var authorizeRecords = reactive([]);
/**
 * 获取权限详情
 *
 * @param id 权限ID
 */
var handleGetPermissionDetail = function (id) {
    getPermissionDetail(id, {
        keyword: authorizedPrincipalSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            permissionId.value = data.permissionId;
            permissionName.value = data.permissionName;
            permissionLocator.value = data.permissionLocator;
            permissionInfoForm.id = data.permissionId;
            permissionInfoForm.code = data.permissionCode;
            permissionInfoForm.name = data.permissionName;
            permissionInfoForm.desc = data.permissionDesc;
            authorizeRecords.length = 0;
            authorizeRecords.push.apply(authorizeRecords, data.authorizeRecords);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取权限详情");
    });
};
/** 被授权主体搜索关键字 */
var authorizedPrincipalSearchKeyword = ref("");
/**
 * 搜索被授权主体
 */
var handleSerachAuthorizedPrincipal = function () {
    handleGetPermissionDetail(permissionId.value);
};
/**
 * 跳转被授权主体详情
 */
var handeToPrincipalDetail = function (principal) {
    if (principal.principalType === "USER") {
        handleToUserDetail(principal.principalId);
    }
    if (principal.principalType === "USER_GROUP") {
        hantoToUserGroupDetail(principal.principalId);
    }
    if (principal.principalType === "ROLE") {
        handleToRoleDetail(principal.principalId);
    }
};
/**
 * 跳转用户组详情
 */
var hantoToUserGroupDetail = function (id) {
    router.push({
        path: "/user/group/detail",
        query: {
            id: id,
            active_tab: "user_group_info",
        },
    });
};
/**
 * 跳转用户详情
 */
var handleToUserDetail = function (id) {
    router.push({
        path: "/user/detail",
        query: {
            id: id,
            active_tab: "user_info",
        },
    });
};
/**
 * 跳转角色详情
 */
var handleToRoleDetail = function (id) {
    router.push({
        path: "/role/detail",
        query: {
            id: id,
            active_tab: "role_info",
        },
    });
};
/** 添加限制条件对话框 */
var addAuthorizeConditionModalVisible = ref(false);
var addAuthorizeConditionFormRef = ref();
var addAuthorizeConditionForm = reactive({
    authorizeIds: undefined,
    permissionExpIds: undefined,
});
var addAuthorizeConditionFormRules = {
    permissionExpIds: [{ required: true, message: "请选择限制条件" }],
};
/** 限制条件列表 */
var authorizeConditionList = reactive([]);
var authorizeConditionListPagination = {
    total: 0,
    current: 1,
};
var authorizeConditionSearchKeyword = ref("");
/**
 * 打开添加限制条件对话框
 */
var handleOpenAddAuthorizeConditionModal = function (authorizeRecord) {
    addAuthorizeConditionModalVisible.value = true;
    addAuthorizeConditionForm.authorizeIds = [authorizeRecord.authorizeId];
    handleGetAuthorizeConditionList();
};
/**
 * 关闭添加限制条件对话框
 */
var handleCloseAddAuthorizeConditionModal = function () {
    addAuthorizeConditionModalVisible.value = false;
    addAuthorizeConditionFormRef.value.resetFields();
};
/**
 * 获取限制条件
 */
var handleGetAuthorizeConditionList = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    getPermissionExpList({
        page: page,
        size: size,
        keyword: authorizeConditionSearchKeyword.value,
    })
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            if (page == 1) {
                authorizeConditionList.length = 0;
                authorizeConditionList.push.apply(authorizeConditionList, data.list);
            }
            else {
                authorizeConditionList.push.apply(authorizeConditionList, data.list);
            }
            authorizeConditionListPagination.current = data.current;
            authorizeConditionListPagination.total = data.total;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取权限表达式列表");
    });
};
var loadMoreAuthorizeConditionLoading = false;
/**
 * 加载更多限制条件
 */
var loadMoreAuthorizeCondition = function () {
    if (loadMoreAuthorizeConditionLoading)
        return;
    if (authorizeConditionList.length < authorizeConditionListPagination.total) {
        loadMoreAuthorizeConditionLoading = true;
        authorizeConditionListPagination.current++;
        handleGetAuthorizeConditionList(authorizeConditionListPagination.current);
        loadMoreAuthorizeConditionLoading = false;
    }
};
/**
 * 搜索限制条件
 */
var handleSearchAuthorizeCondition = function () {
    handleGetAuthorizeConditionList(1);
};
var addAuthorizeConditionFormSubmitLoading = ref(false);
/**
 * 添加限制条件表单提交
 */
var handleAddAuthorizeConditionFormSubmit = function () {
    addAuthorizeConditionFormSubmitLoading.value = true;
    addAuthorizeCondition(addAuthorizeConditionForm)
        .then(function (result) {
        handleApiSuccess(result, function () {
            handleCloseAddAuthorizeConditionModal();
            handleGetPermissionDetail(permissionId.value);
        });
    })
        .catch(function (err) {
        handleApiError(err, "添加限制条件");
    })
        .finally(function () {
        addAuthorizeConditionFormSubmitLoading.value = false;
    });
};
/**
 * 取消限制
 */
var handleRemoveAuthorizeCondition = function (authoruzeRecord, permissionExp) {
    Modal.confirm({
        title: "\u786E\u5B9A\u53D6\u6D88\u9650\u5236\u6761\u4EF6\u300C".concat(permissionExp.name, "\u300D\u7684\u9650\u5236\u5417\uFF1F"),
        content: "此操作将不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            removeAuthorizeCondition({
                authorizeIds: [authoruzeRecord.authorizeId],
                permissionExpIds: [permissionExp.id],
            })
                .then(function (result) {
                handleApiSuccess(result, function () {
                    handleGetPermissionDetail(permissionId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "取消限制");
            });
        },
    });
};
/**
 * 取消授权
 */
var handleCancelAuthorization = function (principalId) {
    Modal.warning({
        title: "\u786E\u5B9A\u53D6\u6D88\u5BF9\u6743\u9650\u300C".concat(permissionName.value, "\u300D\u7684\u6388\u6743\u5417\uFF1F"),
        content: "此操作不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "danger",
        },
        onOk: function () {
            cancelAuthorization(permissionId.value, principalId)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("取消授权成功");
                    handleGetPermissionDetail(permissionId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "取消授权");
            });
        },
    });
};
/**
 * 更新授权优先级
 */
var handleUpdateAuthorizePriority = function (record) {
    var priorityText = "";
    switch (record.priority) {
        case -1:
            priorityText = "最低";
            break;
        case 0:
            priorityText = "低";
            break;
        case 1:
            priorityText = "中";
            break;
        case 2:
            priorityText = "高";
            break;
        case 3:
            priorityText = "最高";
            break;
    }
    Modal.warning({
        title: "\u786E\u5B9A\u5C06\u6388\u6743\u4F18\u5148\u7EA7\u8C03\u6574\u4E3A\u300C".concat(priorityText, "\u300D\u5417\uFF1F"),
        content: "此操作不可恢复，请谨慎操作。",
        hideCancel: false,
        okButtonProps: {
            status: "warning",
        },
        onOk: function () {
            updateAuthorizePriority(record.authorizeId, record.priority)
                .then(function (result) {
                handleApiSuccess(result, function () {
                    Notification.success("更新授权优先级成功");
                    handleGetPermissionDetail(permissionId.value);
                });
            })
                .catch(function (err) {
                handleApiError(err, "更新授权优先级");
            });
        },
    });
};
export default defineComponent({
    setup: function () {
        onMounted(function () {
            activeTab.value = getQueryString("active_tab") || "permission_info";
            var permissionId = getQueryString("id");
            handleGetPermissionDetail(permissionId);
        });
        return {
            handleBack: handleBack,
            activeTab: activeTab,
            handleTabChange: handleTabChange,
            permissionId: permissionId,
            permissionName: permissionName,
            permissionLocator: permissionLocator,
            permissionInfoFormRef: permissionInfoFormRef,
            permissionInfoForm: permissionInfoForm,
            permissionInfoFormRules: permissionInfoFormRules,
            authorizedPrincipalSearchKeyword: authorizedPrincipalSearchKeyword,
            handleSerachAuthorizedPrincipal: handleSerachAuthorizedPrincipal,
            authorizeRecords: authorizeRecords,
            handleResetPermissionInfoForm: handleResetPermissionInfoForm,
            permissionInfoFormSubmit: permissionInfoFormSubmit,
            handeToPrincipalDetail: handeToPrincipalDetail,
            addAuthorizeConditionModalVisible: addAuthorizeConditionModalVisible,
            addAuthorizeConditionFormRef: addAuthorizeConditionFormRef,
            addAuthorizeConditionForm: addAuthorizeConditionForm,
            addAuthorizeConditionFormRules: addAuthorizeConditionFormRules,
            handleOpenAddAuthorizeConditionModal: handleOpenAddAuthorizeConditionModal,
            handleCloseAddAuthorizeConditionModal: handleCloseAddAuthorizeConditionModal,
            authorizeConditionList: authorizeConditionList,
            authorizeConditionListPagination: authorizeConditionListPagination,
            authorizeConditionSearchKeyword: authorizeConditionSearchKeyword,
            handleSearchAuthorizeCondition: handleSearchAuthorizeCondition,
            loadMoreAuthorizeCondition: loadMoreAuthorizeCondition,
            addAuthorizeConditionFormSubmitLoading: addAuthorizeConditionFormSubmitLoading,
            handleAddAuthorizeConditionFormSubmit: handleAddAuthorizeConditionFormSubmit,
            handleRemoveAuthorizeCondition: handleRemoveAuthorizeCondition,
            handleCancelAuthorization: handleCancelAuthorization,
            handleUpdateAuthorizePriority: handleUpdateAuthorizePriority,
        };
    },
});
