import {
    type AsyncTask,
    downloadTaskResult,
    getTask,
    getTaskList,
    type TaskStatus,
    type TaskType,
} from "@/api/asyncTask";
import {Message} from "@arco-design/web-vue";
import {defineComponent, onMounted, onUnmounted, ref} from "vue";
import {usePagination} from "@/hooks/usePagination";
import {type TaskNotificationMessage} from "@/hooks/useTaskNotification";
import {taskEmitter} from "@/hooks/taskEmitter";
import {handleApiError, handleApiSuccess} from "@/util/tool";

/** 加载状态 */
const loading = ref(false);

/** 任务列表 */
const taskList = ref<AsyncTask[]>([]);

/** 任务类型筛选 */
const taskTypeFilter = ref<TaskType | "">("");

/** 任务状态筛选 */
const statusFilter = ref<TaskStatus | "">("");

/** 详情抽屉可见性 */
const detailVisible = ref(false);

/** 错误详情弹窗可见性 */
const errorDetailVisible = ref(false);

/** 当前任务 */
const currentTask = ref<AsyncTask | null>(null);

/** 解析后的任务结果 */
const parsedTaskResult = ref<any>(null);

/** 解析后的任务参数 */
const parsedTaskParams = ref<any>(null);

/**
 * 任务通知回调
 */
const handleTaskNotification = (message: TaskNotificationMessage) => {
  // 更新任务列表中的任务状态
  const index = taskList.value.findIndex(
    (task) => task.taskId === message.taskId,
  );
  if (index !== -1) {
    taskList.value[index].status = message.status as any;
    taskList.value[index].progress = message.progress;
    taskList.value[index].errorMessage = message.errorMessage;
    taskList.value[index].resultFileName = message.resultFileName;
  } else {
    // 如果列表中没有该任务，刷新列表
    fetchTaskList();
  }
};

/**
 * 初始化任务通知监听
 */
const initTaskNotification = () => {
  taskEmitter.on("task:update", handleTaskNotification);
};

/**
 * 移除任务通知监听
 */
const removeTaskNotification = () => {
  taskEmitter.off("task:update", handleTaskNotification);
};

/**
 * 获取任务列表
 */
const fetchTaskList = async (pageData?: { page: number; size: number }) => {
  loading.value = true;
  try {
    const page = pageData?.page || 1;
    const pageSize = pageData?.size || 15;
    const params: any = {
      page,
      pageSize,
    };
    if (taskTypeFilter.value) {
      params.taskType = taskTypeFilter.value;
    }
    if (statusFilter.value) {
      params.status = statusFilter.value;
    }
    const res: any = await getTaskList(params);
    handleApiSuccess(res, (data: any) => {
      taskList.value = data.list || [];
      taskListPagination.updatePagination(page, data.total || 0, pageSize);
    });
  } catch (error) {
    handleApiError(error, "获取任务列表");
  } finally {
    loading.value = false;
  }
};

/** 分页 hook */
let taskListPagination;

/**
 * 搜索
 */
const handleSearch = () => {
  fetchTaskList({ page: 1, size: taskListPagination.pagination.pageSize });
};

/**
 * 重置
 */
const handleReset = () => {
  taskTypeFilter.value = "";
  statusFilter.value = "";
  fetchTaskList({ page: 1, size: taskListPagination.pagination.pageSize });
};

/**
 * 分页变化
 */
const handlePageChange = (page: number) => {
  taskListPagination.handlePageChange(page);
};

/**
 * 每页数量变化
 */
const handlePageSizeChange = (pageSize: number) => {
  taskListPagination.handlePageSizeChange(pageSize);
};

/**
 * 查看任务详情
 */
const handleView = async (record: AsyncTask) => {
  try {
    const res: any = await getTask(record.taskId);
    handleApiSuccess(res, (data: any) => {
      currentTask.value = data;
      // 解析任务结果 JSON
      if (data.taskResult) {
        try {
          parsedTaskResult.value = JSON.parse(data.taskResult);
        } catch {
          parsedTaskResult.value = null;
        }
      } else {
        parsedTaskResult.value = null;
      }
      // 解析任务参数 JSON
      if (data.taskParams) {
        try {
          parsedTaskParams.value = JSON.parse(data.taskParams);
        } catch {
          parsedTaskParams.value = null;
        }
      } else {
        parsedTaskParams.value = null;
      }
    });
    detailVisible.value = true;
  } catch (error) {
    handleApiError(error, "获取任务详情");
  }
};

/**
 * 显示错误详情
 */
const handleShowErrorDetail = () => {
  errorDetailVisible.value = true;
};

/**
 * 下载任务结果
 */
const handleDownload = async (record: AsyncTask) => {
  try {
    const res: any = await downloadTaskResult(record.taskId);
    const blob = res.data || res;
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = record.resultFileName || "result.xlsx";
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    window.URL.revokeObjectURL(url);
  } catch (error) {
    console.error("下载失败", error);
    Message.error("下载失败");
  }
};

/**
 * 任务类型选项 - 由数据驱动
 */
const taskTypeOptions: { label: string; value: TaskType }[] = [
  { label: "用户导入", value: "USER_IMPORT" },
  { label: "用户导出", value: "USER_EXPORT" },
];

/**
 * 任务状态选项
 */
const statusOptions = [
  { label: "待执行", value: "PENDING" },
  { label: "执行中", value: "RUNNING" },
  { label: "成功", value: "SUCCESS" },
  { label: "失败", value: "FAILED" },
  { label: "已取消", value: "CANCELLED" },
];

/**
 * 获取任务类型名称
 */
const getTaskTypeName = (type?: string): string => {
  const item = taskTypeOptions.find((option) => option.value === type);
  return item ? item.label : type || "-";
};

/**
 * 获取任务状态名称
 */
const getStatusName = (status?: string): string => {
  const item = statusOptions.find((option) => option.value === status);
  return item ? item.label : status || "-";
};

/**
 * 获取任务状态颜色
 */
const getStatusColor = (status?: string): string => {
  const map: Record<string, string> = {
    PENDING: "gray",
    RUNNING: "blue",
    SUCCESS: "green",
    FAILED: "red",
    CANCELLED: "orange",
  };
  return status ? map[status] || "gray" : "gray";
};

/**
 * 格式化执行耗时（添加单位）
 */
const formatDuration = (ms?: number): string => {
  if (!ms && ms !== 0) return "-";
  if (ms < 1000) return `${ms} 毫秒`;
  if (ms < 60000) return `${(ms / 1000).toFixed(1)} 秒`;
  return `${(ms / 60000).toFixed(1)} 分钟`;
};

export default defineComponent({
  setup() {
    taskListPagination = usePagination("taskList", fetchTaskList);

    // 初始化任务通知监听
    onMounted(() => {
      initTaskNotification();
    });

    // 移除任务通知监听
    onUnmounted(() => {
      removeTaskNotification();
    });

    return {
      loading,
      taskList,
      taskTypeFilter,
      statusFilter,
      taskTypeOptions,
      statusOptions,
      taskListPagination,
      detailVisible,
      errorDetailVisible,
      currentTask,
      parsedTaskResult,
      parsedTaskParams,
      handleSearch,
      handleReset,
      handlePageChange,
      handlePageSizeChange,
      handleView,
      handleShowErrorDetail,
      handleDownload,
      getTaskTypeName,
      getStatusName,
      getStatusColor,
      formatDuration,
    };
  },
});
