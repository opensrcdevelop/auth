package cn.opensrcdevelop.common.util;

import com.alibaba.ttl.TtlCallable;
import com.alibaba.ttl.TtlRunnable;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Future;
import lombok.extern.slf4j.Slf4j;
import org.springframework.lang.NonNull;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.security.core.context.SecurityContextHolder;

@Slf4j
public class TtlThreadPoolTaskExecutor extends ThreadPoolTaskExecutor {

    private final ConcurrentHashMap<String, Long> startTimes = new ConcurrentHashMap<>();

    private final String executorName;

    public TtlThreadPoolTaskExecutor(String executorName) {
        this.executorName = executorName;
        this.setThreadNamePrefix(executorName + "-");
    }

    @Override
    public void execute(@NonNull Runnable runnable) {
        Runnable ttlRunnable = TtlRunnable.get(runnable);
        super.execute(ttlRunnable);
    }

    @Override
    @NonNull
    public <T> Future<T> submit(@NonNull Callable<T> task) {
        Callable<T> ttlCallable = TtlCallable.get(task);
        return super.submit(ttlCallable);
    }

    @Override
    @NonNull
    public Future<?> submit(@NonNull Runnable task) {
        Runnable ttlRunnable = TtlRunnable.get(task);
        return super.submit(ttlRunnable);
    }

    @Override
    protected void beforeExecute(@NonNull Thread thread, Runnable task) {
        startTimes.put(String.valueOf(task.hashCode()), System.currentTimeMillis());
    }

    @Override
    protected void afterExecute(Runnable task, Throwable ex) {
        Long startTime = startTimes.remove(String.valueOf(task.hashCode()));
        long diff = System.currentTimeMillis() - startTime;
        log.info(
                "{}-monitor: Duration: {} ms, PoolSize: {}, CorePoolSize: {}, Active: {}, Queue: {}, MaximumPoolSize: {},  KeepAliveTime: {}",
                executorName, diff, this.getPoolSize(), this.getCorePoolSize(), this.getActiveCount(),
                this.getQueueSize(), this.getMaxPoolSize(), this.getKeepAliveSeconds());
        // 清理 SecurityContext，防止线程复用时泄露用户身份
        SecurityContextHolder.clearContext();
    }
}
