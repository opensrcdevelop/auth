import apiRequest from "@/util/apiRequest";

/**
 * 任务状态
 */
export type TaskStatus = "PENDING" | "RUNNING" | "SUCCESS" | "FAILED" | "CANCELLED";

/**
 * 任务状态选项
 */
export const TASK_STATUS_OPTIONS: { label: string; value: TaskStatus }[] = [
  { label: "等待中", value: "PENDING" },
  { label: "执行中", value: "RUNNING" },
  { label: "成功", value: "SUCCESS" },
  { label: "失败", value: "FAILED" },
  { label: "已取消", value: "CANCELLED" },
];

/**
 * 任务类型
 */
export type TaskType = "USER_IMPORT" | "USER_EXPORT";

/**
 * 任务类型配置（由数据驱动）
 */
export const TASK_TYPE_CONFIG: Record<TaskType, { label: string; value: TaskType }> = {
  USER_IMPORT: { label: "用户导入", value: "USER_IMPORT" },
  USER_EXPORT: { label: "用户导出", value: "USER_EXPORT" },
};

/**
 * 异步任务信息
 */
export interface AsyncTask {
  taskId: string;
  taskType: TaskType;
  taskName: string;
  status: TaskStatus;
  taskParams?: string;
  taskResult?: string;
  resultFilePath?: string;
  resultFileName?: string;
  errorMessage?: string;
  progress: number;
  createTime: string;
  startTime?: string;
  endTime?: string;
  duration?: number;
}

/**
 * 分页查询任务列表参数
 */
export interface TaskListParams {
  page?: number;
  pageSize?: number;
  taskType?: TaskType;
  status?: TaskStatus;
}

/**
 * 分页查询任务列表响应（包含外层包装）
 */
export interface TaskListResponse {
  success: boolean;
  code: number;
  message: string;
  time: string;
  data: {
    list: AsyncTask[];
    total: number;
    current: number;
    size: number;
    pages: number;
  };
}

/**
 * 任务提交响应
 */
export interface TaskSubmitResponse {
  taskId: string;
}

/**
 * 获取任务详情
 *
 * @param taskId 任务ID
 * @returns 任务详情
 */
export function getTask(taskId: string) {
  return apiRequest.get({
    url: `/task/${taskId}`,
  });
}

/**
 * 分页查询任务列表
 *
 * @param params 查询参数
 * @returns 任务列表
 */
export function getTaskList(params: TaskListParams) {
  return apiRequest.get({
    url: "/task/list",
    params,
  });
}

/**
 * 下载任务结果文件
 *
 * @param taskId 任务ID
 * @returns Blob
 */
export function downloadTaskResult(taskId: string) {
  return apiRequest.get({
    url: `/task/${taskId}/download`,
    responseType: "blob",
  });
}
