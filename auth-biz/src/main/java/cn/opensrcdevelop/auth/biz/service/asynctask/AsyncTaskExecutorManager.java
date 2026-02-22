package cn.opensrcdevelop.auth.biz.service.asynctask;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * 异步任务执行器管理器
 */
@Slf4j
@Component
public class AsyncTaskExecutorManager {

    private final Map<String, AsyncTaskExecutor> executors = new ConcurrentHashMap<>();

    /**
     * 注册任务执行器
     *
     * @param executor
     *            任务执行器
     */
    public void register(AsyncTaskExecutor executor) {
        executors.put(executor.getTaskType(), executor);
        log.info("注册异步任务执行器: taskType={}", executor.getTaskType());
    }

    /**
     * 获取任务执行器
     *
     * @param taskType
     *            任务类型
     * @return 任务执行器
     */
    public AsyncTaskExecutor getExecutor(String taskType) {
        return executors.get(taskType);
    }

    /**
     * 检查任务类型是否支持
     *
     * @param taskType
     *            任务类型
     * @return 是否支持
     */
    public boolean isSupported(String taskType) {
        return executors.containsKey(taskType);
    }
}
