package cn.opensrcdevelop.auth.biz.service.asynctask;

import cn.opensrcdevelop.auth.biz.service.user.excel.UserExportAsyncTaskExecutor;
import cn.opensrcdevelop.auth.biz.service.user.excel.UserImportAsyncTaskExecutor;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 异步任务执行器自动注册器
 */
@Slf4j
@Component
@RequiredArgsConstructor
public class AsyncTaskExecutorAutoConfiguration {

    private final AsyncTaskExecutorManager executorManager;
    private final UserImportAsyncTaskExecutor userImportAsyncTaskExecutor;
    private final UserExportAsyncTaskExecutor userExportAsyncTaskExecutor;

    @PostConstruct
    public void registerExecutors() {
        // 注册用户导入执行器
        executorManager.register(userImportAsyncTaskExecutor);

        // 注册用户导出执行器
        executorManager.register(userExportAsyncTaskExecutor);

        log.info("异步任务执行器自动注册完成");
    }
}
