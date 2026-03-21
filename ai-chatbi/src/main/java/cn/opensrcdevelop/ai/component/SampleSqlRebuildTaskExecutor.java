package cn.opensrcdevelop.ai.component;

import cn.opensrcdevelop.ai.service.SampleSqlService;
import cn.opensrcdevelop.auth.biz.enums.AsyncTaskType;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutor;
import cn.opensrcdevelop.auth.biz.service.asynctask.AsyncTaskExecutorAnno;
import cn.opensrcdevelop.common.exception.ServerException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 示例 SQL 重建索引任务执行器
 */
@Slf4j
@Component
@RequiredArgsConstructor
@AsyncTaskExecutorAnno(taskType = "SAMPLE_SQL_REBUILD")
public class SampleSqlRebuildTaskExecutor implements AsyncTaskExecutor {

    public static final String TASK_NAME = "示例 SQL 重建索引";

    private final SampleSqlService sampleSqlService;

    @Override
    public String getTaskType() {
        return AsyncTaskType.SAMPLE_SQL_REBUILD.getCode();
    }

    @Override
    public void execute(String taskId, String taskParams, TaskExecutionContext context) {
        try {
            log.info("开始执行示例 SQL 重建索引任务: taskId={}", taskId);
            context.updateProgress(50);

            sampleSqlService.rebuildIndex();

            context.setResult("索引重建成功");
            log.info("示例 SQL 重建索引任务完成: taskId={}", taskId);
        } catch (Exception e) {
            log.error("示例 SQL 重建索引任务执行失败: taskId={}", taskId, e);
            throw new ServerException("重建失败: " + e.getMessage(), e);
        }
    }
}
