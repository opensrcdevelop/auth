package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.service.SampleSqlService;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutor;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutorAnno;
import cn.opensrcdevelop.common.exception.ServerException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 示例 SQL 同步任务执行器
 */
@Slf4j
@Component
@RequiredArgsConstructor
@AsyncTaskExecutorAnno(taskType = "SAMPLE_SQL_SYNC")
public class SampleSqlSyncTaskExecutor implements AsyncTaskExecutor {

    public static final String TASK_NAME = "示例 SQL 同步";

    private final SampleSqlService sampleSqlService;

    @Override
    public String getTaskType() {
        return AsyncTaskType.SAMPLE_SQL_SYNC.getCode();
    }

    @Override
    public void execute(String taskId, String taskParams, TaskExecutionContext context) {
        try {
            log.info("开始执行示例 SQL 同步任务: taskId={}", taskId);
            context.updateProgress(50);

            int count = sampleSqlService.syncFromLikes();

            context.setResult("同步了 " + count + " 条示例 SQL");
            log.info("示例 SQL 同步任务完成: taskId={}, count={}", taskId, count);
        } catch (Exception e) {
            log.error("示例 SQL 同步任务执行失败: taskId={}", taskId, e);
            throw new ServerException("同步失败: " + e.getMessage(), e);
        }
    }
}
