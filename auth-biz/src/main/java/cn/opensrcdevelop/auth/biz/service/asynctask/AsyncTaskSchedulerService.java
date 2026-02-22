package cn.opensrcdevelop.auth.biz.service.asynctask;

import cn.opensrcdevelop.auth.biz.constants.MessageConstants;
import cn.opensrcdevelop.auth.biz.entity.asynctask.AsyncTask;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskStatus;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.mapper.asynctask.AsyncTaskMapper;
import cn.opensrcdevelop.auth.biz.service.asynctask.storage.StorageService;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import jakarta.annotation.Resource;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.Map;

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

    @Resource
    @Lazy
    private AsyncTaskSchedulerService schedulerService;

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
            // 1. 检查并行度
            AsyncTaskType asyncTaskType = AsyncTaskType.fromCode(taskType);
            if (asyncTaskType != null) {
                int maxParallelism = asyncTaskType.getMaxParallelism();
                long runningCount = asyncTaskService.countRunningTasks(taskType);
                if (runningCount >= maxParallelism) {
                    log.warn("任务类型 {} 当前运行数量 {} 已达到最大并行度 {}",
                            taskType, runningCount, maxParallelism);
                    throw new BizException(MessageConstants.ASYNC_TASK_MSG_1000);
                }
            }

            // 2. 创建任务记录
            String taskParamsJson = CommonUtil.nonJdkSerializeObject(taskParams);
            String taskId = asyncTaskService.createTask(taskType, taskName, taskParamsJson, userId);

            // 3. 异步执行任务
            schedulerService.executeTaskAsync(taskId, taskType, taskParamsJson);

            log.info("异步任务已提交: taskId={}, taskType={}, taskName={}", taskId, taskType, taskName);

            return taskId;
        } catch (BizException e) {
            throw e;
        } catch (Exception e) {
            log.error("提交异步任务失败: taskType={}, taskName={}", taskType, taskName, e);
            throw new ServerException("提交异步任务失败", e);
        }
    }

    /**
     * 异步执行任务
     */
    @Async(ExecutorConstants.EXECUTOR_IO_DENSE)
    public void executeTaskAsync(String taskId, String taskType, String taskParams) {
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
                    taskId, asyncTaskService, asyncTaskMapper, storageService);

            // 执行任务
            executor.execute(taskId, taskParams, context);

            // 如果任务未完成（未设置结果），自动标记为成功
            task = asyncTaskMapper.selectById(taskId);
            if (task != null && AsyncTaskStatus.RUNNING.getCode().equals(task.getStatus())) {
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

        @Getter
        private String result;
        @Getter
        private String resultFilePath;
        @Getter
        private String resultFileName;

        public TaskExecutionContextImpl(String taskId, AsyncTaskService asyncTaskService,
                AsyncTaskMapper asyncTaskMapper,
                StorageService storageService) {
            this.taskId = taskId;
            this.asyncTaskService = asyncTaskService;
            this.asyncTaskMapper = asyncTaskMapper;
            this.storageService = storageService;
        }

        @Override
        public void updateProgress(int progress) {
            asyncTaskService.updateProgress(taskId, progress);
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
    }
}
