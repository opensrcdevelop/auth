package cn.opensrcdevelop.auth.biz.component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.TaskScheduler;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;

@Component
@RequiredArgsConstructor
@Slf4j
public class ScheduledTaskService {

    private final TaskScheduler taskScheduler;
    private final Map<String, ScheduledFuture<?>> scheduledTasks = new ConcurrentHashMap<>();

    /**
     * 添加定时任务
     *
     * @param taskName 任务名称
     * @param task 任务
     * @param executeTime 执行时间
     * @return
     */
    @SuppressWarnings("all")
    public ScheduledFuture<?> addTaskAtFixedTime(String taskName, Runnable task, LocalDateTime executeTime) {
        ScheduledFuture<?> scheduledFuture = taskScheduler.schedule(task, executeTime.atZone(ZoneId.systemDefault()).toInstant());
        scheduledTasks.put(taskName, scheduledFuture);
        log.info("成功添加任务：{}, 执行时间：{}", taskName, executeTime);
        return scheduledFuture;
    }

    /**
     *  取消定时任务
     *
     * @param taskName 任务名称
     */
    public void cancelTask(String taskName) {
        Optional.ofNullable(scheduledTasks.get(taskName)).ifPresent(taskFuture -> {
            if (taskFuture.isCancelled()) {
                log.warn("任务：{} 已被取消", taskName);
                scheduledTasks.remove(taskName);
                return;
            }

            if (taskFuture.isDone()) {
                log.warn("任务：{} 已完成", taskName);
                scheduledTasks.remove(taskName);
                return;
            }
            taskFuture.cancel(true);
            scheduledTasks.remove(taskName);
            log.info("成功取消任务：{}", taskName);
        });
    }
}
