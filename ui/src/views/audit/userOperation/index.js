import { getObjChanges, getUserOperationLogs, } from "@/api/auditLog";
import { usePagination } from "@/hooks/usePagination";
import router from "@/router";
import { handleApiError, handleApiSuccess } from "@/util/tool";
import dayjs from "dayjs";
import { defineComponent, reactive, ref } from "vue";
/**
 * 操作类型
 */
var operationTypes = [
    { value: 1, label: "修改个人信息" },
    { value: 2, label: "修改密码" },
    { value: 3, label: "绑定邮箱" },
    { value: 4, label: "解绑邮箱" },
    { value: 5, label: "绑定手机号" },
    { value: 6, label: "解绑手机号" },
    { value: 7, label: "绑定第三方账号" },
    { value: 8, label: "解绑第三方账号" },
    { value: 9, label: "绑定 MFA 设备" },
    { value: 10, label: "重置密码" },
    { value: 11, label: "ChatBI 对话" },
    { value: 12, label: "ChatBI 问答反馈" },
    { value: 13, label: "ChatBI 删除对话历史" },
    { value: 14, label: "ChatBI 更新对话历史" },
];
/**
 * 表格列
 */
var columns = reactive([
    {
        label: "用户",
        key: "username",
        value: function (data) { return data.username; },
        visible: true,
        editable: false,
        width: 100,
        ellipsis: false,
    },
    {
        label: "详情",
        key: "detail",
        visible: true,
        editable: false,
        width: 380,
        ellipsis: true,
    },
    {
        label: "类型",
        key: "type",
        visible: false,
        editable: true,
        width: 100,
        ellipsis: false,
    },
    {
        label: "结果",
        key: "result",
        visible: true,
        editable: true,
        ellipsis: false,
        width: 60,
    },
    {
        label: "IP",
        key: "ip",
        visible: false,
        editable: true,
        ellipsis: false,
        width: 100,
    },
    {
        label: "IP 归属地",
        key: "ipRegion",
        visible: false,
        editable: true,
        ellipsis: false,
        width: 120,
    },
    {
        label: "设备类型",
        key: "deviceType",
        visible: false,
        editable: true,
        ellipsis: false,
        width: 100,
    },
    {
        label: "OS 类型",
        key: "osType",
        visible: false,
        editable: true,
        ellipsis: false,
        width: 140,
    },
    {
        label: "浏览器类型",
        key: "browserType",
        visible: false,
        editable: true,
        ellipsis: false,
        width: 140,
    },
    {
        label: "时间",
        key: "time",
        visible: true,
        editable: true,
        ellipsis: false,
        width: 180,
    },
]);
/**
 * 获取 Dayjs 对象
 */
var getDayjs = function (current) {
    return dayjs(current);
};
// 用户操作日志
var userOperationLogs = reactive([]);
var userOperationLogsPagination;
var searchKeyword = ref(null);
var operationType = ref(null);
var dateRange = ref(null);
/**
 * 获取用户操作日志
 */
var handleGetUserOperationLogs = function (page, size) {
    if (page === void 0) { page = 1; }
    if (size === void 0) { size = 15; }
    var params = {
        page: page,
        size: size,
        keyword: undefined,
        type: undefined,
        start: undefined,
        end: undefined,
    };
    if (searchKeyword.value) {
        params.keyword = searchKeyword.value.trim();
    }
    if (operationType.value) {
        params.type = operationType.value;
    }
    if (dateRange.value) {
        params.start = dateRange.value[0];
        params.end = dateRange.value[1];
    }
    getUserOperationLogs(params)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            userOperationLogs.length = 0;
            userOperationLogs.push.apply(userOperationLogs, data.list);
            userOperationLogsPagination.updatePagination(data.current, data.total, data.size);
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取用户操作日志");
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
        },
    });
};
/**
 * 对象变更日志
 */
var objChanges = reactive([]);
var objChangesModalVisible = ref(false);
var handleGetObjChanges = function (auditLogId) {
    getObjChanges(auditLogId)
        .then(function (result) {
        handleApiSuccess(result, function (data) {
            objChanges.length = 0;
            objChanges.push.apply(objChanges, data.changes);
            objChangesModalVisible.value = true;
        });
    })
        .catch(function (err) {
        handleApiError(err, "获取对象变更日志");
    });
};
export default defineComponent({
    setup: function () {
        userOperationLogsPagination = usePagination("userOperationLogs", function (_a) {
            var page = _a.page, size = _a.size;
            handleGetUserOperationLogs(page, size);
        });
        return {
            operationTypes: operationTypes,
            columns: columns,
            searchKeyword: searchKeyword,
            operationType: operationType,
            dateRange: dateRange,
            userOperationLogs: userOperationLogs,
            userOperationLogsPagination: userOperationLogsPagination,
            getDayjs: getDayjs,
            handleGetUserOperationLogs: handleGetUserOperationLogs,
            handleToUserDetail: handleToUserDetail,
            handleGetObjChanges: handleGetObjChanges,
            objChanges: objChanges,
            objChangesModalVisible: objChangesModalVisible,
        };
    },
});
