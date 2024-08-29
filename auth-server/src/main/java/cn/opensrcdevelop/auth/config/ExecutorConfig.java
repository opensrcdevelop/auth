package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.util.TtlThreadPoolTaskExecutor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;

@Configuration
@EnableAsync
public class ExecutorConfig {

    @Bean(ExecutorConstants.EXECUTOR_IO_DENSE)
    public Executor ioDenseAsyncServiceExecutor() {
        ThreadPoolTaskExecutor executor = new TtlThreadPoolTaskExecutor("ioDenseAsyncServiceExecutor");
        int cores = Runtime.getRuntime().availableProcessors();
        // 核心线程数
        executor.setCorePoolSize(cores * 80);
        // 最大线程数
        executor.setMaxPoolSize(cores * 200);
        executor.setKeepAliveSeconds(30);
        // 队列大小
        executor.setQueueCapacity(5);
        executor.setRejectedExecutionHandler(new ThreadPoolExecutor.AbortPolicy());
        executor.setWaitForTasksToCompleteOnShutdown(true);
        executor.initialize();
        return executor;
    }
}
