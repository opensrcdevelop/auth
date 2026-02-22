package cn.opensrcdevelop.auth.biz.service.asynctask;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.entity.asynctask.AsyncTask;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.mapper.asynctask.AsyncTaskMapper;
import cn.opensrcdevelop.auth.biz.service.asynctask.storage.StorageService;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.exception.BizException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

/**
 * 异步任务调度服务
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AsyncTaskSchedulerService {

    private final AsyncTaskService asyncTaskService;
    private final AsyncTaskMapper asyncTaskMapper;
    private final AsyncTaskExecutorManager executorManager;
    private final AsyncTaskNotificationService notificationService;
    private final StorageService storageService;
    private final ObjectMapper objectMapper;

    /**
     * 提交异步任务
     *
     * @param taskType
     *            任务类型
     * @param taskName
     *            任务名称
     * @param taskParams
     *            任务参数
     * @param userId
     *            用户ID
     * @return 任务ID
     */
    public String submitTask(String taskType, String taskName, Map<String, Object> taskParams, String userId) {
        try {
            // 检查并行度
            AsyncTaskType asyncTaskType = AsyncTaskType.fromCode(taskType);
            if (asyncTaskType != null) {
                int maxParallelism = asyncTaskType.getMaxParallelism();
                long runningCount = asyncTaskService.countRunningTasks(taskType);
                if (runningCount >= maxParallelism) {
                    log.warn("任务类型 {} 当前运行数量 {}已达到最大并行度 {}，请稍后重试",
                            taskType, runningCount, maxParallelism);
                    throw new BizException(MessageConstants.ASYNC_TASK_MSG_1000);
                }
            }

            // 创建任务记录
            String taskParamsJson = objectMapper.writeValueAsString(taskParams);
            String taskId = asyncTaskService.createTask(taskType, taskName, taskParamsJson, userId);

            // 异步执行任务
            executeTaskAsync(taskId, taskType, taskParamsJson, userId);

            log.info("异步任务已提交: taskId={}, taskType={}, taskName={}", taskId, taskType, taskName);

            return taskId;
        } catch (BizException e) {
            throw e;
        } catch (Exception e) {
            log.error("提交异步任务失败: taskType={}, taskName={}", taskType, taskName, e);
            throw new RuntimeException("提交异步任务失败", e);
        }
    }

    /**
     * 异步执行任务
     */
    @Async(ExecutorConstants.EXECUTOR_IO_DENSE)
    public void executeTaskAsync(String taskId, String taskType, String taskParams, String userId) {
        AsyncTaskExecutor executor = executorManager.getExecutor(taskType);
        if (executor == null) {
            log.error("未找到任务执行器: taskId={}, taskType={}", taskId, taskType);
            asyncTaskService.completeTaskFailed(taskId, "未找到任务执行器: " + taskType);
            return;
        }

        AsyncTask task;
        try {
            // 更新任务状态为执行中
            asyncTaskService.startTask(taskId);

            // 创建执行上下文
            TaskExecutionContextImpl context = new TaskExecutionContextImpl(
                    taskId, asyncTaskService, asyncTaskMapper, storageService, notificationService, userId);

            // 执行任务
            executor.execute(taskId, taskParams, context);

            // 如果任务未完成（未设置结果），自动标记为成功
            task = asyncTaskMapper.selectById(taskId);
            if (task != null && "RUNNING".equals(task.getStatus())) {
                asyncTaskService.completeTaskSuccess(taskId, context.getResult(), null, null);
                // 发送通知
                task = asyncTaskMapper.selectById(taskId);
                notificationService.notifyTaskStatusChanged(task);
            }

        } catch (Exception e) {
            log.error("任务执行失败: taskId={}", taskId, e);
            asyncTaskService.completeTaskFailed(taskId, e.getMessage());

            // 发送失败通知
            task = asyncTaskMapper.selectById(taskId);
            if (task != null) {
                notificationService.notifyTaskStatusChanged(task);
            }
        }
    }

    /**
     * 任务执行上下文实现
     */
    private static class TaskExecutionContextImpl implements AsyncTaskExecutor.TaskExecutionContext {

        private final String taskId;
        private final AsyncTaskService asyncTaskService;
        private final AsyncTaskMapper asyncTaskMapper;
        private final StorageService storageService;
        private final AsyncTaskNotificationService notificationService;
        private final String userId;
        private String result;
        private String resultFilePath;
        private String resultFileName;

        public TaskExecutionContextImpl(String taskId, AsyncTaskService asyncTaskService,
                AsyncTaskMapper asyncTaskMapper,
                StorageService storageService,
                AsyncTaskNotificationService notificationService,
                String userId) {
            this.taskId = taskId;
            this.asyncTaskService = asyncTaskService;
            this.asyncTaskMapper = asyncTaskMapper;
            this.storageService = storageService;
            this.notificationService = notificationService;
            this.userId = userId;
        }

        @Override
        public void updateProgress(int progress) {
            asyncTaskService.updateProgress(taskId, progress);
            // 进度更新不发送通知，只在状态变更时发送通知
        }

        @Override
        public void setResult(String result) {
            this.result = result;
        }

        @Override
        public String storeResultFile(byte[] data, String fileName) {
            String filePath = storageService.store(data, fileName);
            this.resultFileName = fileName;
            this.resultFilePath = filePath;

            // 更新任务的文件信息
            AsyncTask task = asyncTaskMapper.selectById(taskId);
            if (task != null) {
                task.setResultFilePath(filePath);
                task.setResultFileName(fileName);
                asyncTaskMapper.updateById(task);
            }

            return filePath;
        }

        public String getResult() {
            return result;
        }

        public String getResultFilePath() {
            return resultFilePath;
        }

        public String getResultFileName() {
            return resultFileName;
        }
    }
}
