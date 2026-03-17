package cn.opensrcdevelop.auth.biz.service.asynctask;

import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 异步任务执行器管理器
 */
@Slf4j
@Component
public class AsyncTaskExecutorManager {

    private final Map<String, AsyncTaskExecutor> executors = new ConcurrentHashMap<>();

    private final List<AsyncTaskExecutor> executorList;

    public AsyncTaskExecutorManager(List<AsyncTaskExecutor> executorList) {
        this.executorList = executorList;
    }

    /**
     * 初始化时自动注册所有执行器
     */
    @PostConstruct
    public void init() {
        for (AsyncTaskExecutor executor : executorList) {
            Class<?> executorClass = executor.getClass();
            AsyncTaskExecutorAnno annotation = AnnotationUtils.findAnnotation(executorClass, AsyncTaskExecutorAnno.class);
            if (annotation != null) {
                String taskType = annotation.taskType();
                executors.put(taskType, executor);
                log.info("通过注解自动注册异步任务执行器: taskType={}, class={}",
                        taskType, executorClass.getName());
            } else {
                // 没有注解的执行器也注册（兼容旧方式）
                String taskType = executor.getTaskType();
                executors.put(taskType, executor);
                log.info("自动注册异步任务执行器: taskType={}, class={}",
                        taskType, executorClass.getName());
            }
        }
    }

    /**
     * 注册任务执行器
     *
     * @param executor
     *            任务执行器
     */
    public void register(AsyncTaskExecutor executor) {
        String taskType = executor.getTaskType();
        executors.put(taskType, executor);
        log.info("注册异步任务执行器: taskType={}", taskType);
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
