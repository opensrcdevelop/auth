import apiRequest from "@/util/apiRequest";
/**
 * 任务状态选项
 */
export var TASK_STATUS_OPTIONS = [
    { label: "等待中", value: "PENDING" },
    { label: "执行中", value: "RUNNING" },
    { label: "成功", value: "SUCCESS" },
    { label: "失败", value: "FAILED" },
    { label: "已取消", value: "CANCELLED" },
];
/**
 * 任务类型配置（由数据驱动）
 */
export var TASK_TYPE_CONFIG = {
    USER_IMPORT: { label: "用户导入", value: "USER_IMPORT" },
    USER_EXPORT: { label: "用户导出", value: "USER_EXPORT" },
};
/**
 * 获取任务详情
 *
 * @param taskId 任务ID
 * @returns 任务详情
 */
export function getTask(taskId) {
    return apiRequest.get({
        url: "/task/".concat(taskId),
    });
}
/**
 * 分页查询任务列表
 *
 * @param params 查询参数
 * @returns 任务列表
 */
export function getTaskList(params) {
    return apiRequest.get({
        url: "/task/list",
        params: params,
    });
}
/**
 * 下载任务结果文件
 *
 * @param taskId 任务ID
 * @returns Blob
 */
export function downloadTaskResult(taskId) {
    return apiRequest.get({
        url: "/task/".concat(taskId, "/download"),
        responseType: "blob",
    });
}
