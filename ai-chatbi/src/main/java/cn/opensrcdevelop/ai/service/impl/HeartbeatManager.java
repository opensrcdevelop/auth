package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.util.SseUtil;
import io.vavr.control.Try;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

/**
 * SSE 心跳管理器
 * 负责调度和取消 SSE 连接的心跳
 */
@Slf4j
@Component
public class HeartbeatManager {

    private static final long HEARTBEAT_INTERVAL_SECONDS = 10;

    private final ScheduledExecutorService scheduler;

    public HeartbeatManager() {
        this.scheduler = java.util.concurrent.Executors.newSingleThreadScheduledExecutor(r -> {
            Thread t = new Thread(r, "sse-heartbeat-scheduler");
            t.setDaemon(true);
            return t;
        });
    }

    /**
     * 启动心跳
     *
     * @param emitter SseEmitter
     * @return ScheduledFuture 用于取消心跳
     */
    public ScheduledFuture<?> startHeartbeat(org.springframework.web.servlet.mvc.method.annotation.SseEmitter emitter) {
        return scheduler.scheduleAtFixedRate(() -> {
            Try.run(() -> SseUtil.sendHeartbeat(emitter));
        }, HEARTBEAT_INTERVAL_SECONDS, HEARTBEAT_INTERVAL_SECONDS, TimeUnit.SECONDS);
    }

    /**
     * 停止心跳
     *
     * @param future ScheduledFuture
     */
    public void stopHeartbeat(ScheduledFuture<?> future) {
        if (future != null && !future.isCancelled()) {
            future.cancel(false);
        }
    }
}
