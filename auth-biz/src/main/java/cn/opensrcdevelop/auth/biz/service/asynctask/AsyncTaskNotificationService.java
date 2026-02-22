package cn.opensrcdevelop.auth.biz.service.asynctask;

import cn.opensrcdevelop.auth.biz.entity.asynctask.AsyncTask;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

/**
 * 异步任务通知服务
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AsyncTaskNotificationService {

    private static final String DESTINATION_FORMAT = "/queue/user/%s/tasks";

    private final SimpMessagingTemplate messagingTemplate;

    /**
     * 通知用户任务状态变更
     *
     * @param task
     *            任务信息
     */
    public void notifyTaskStatusChanged(AsyncTask task) {
        if (task == null || task.getUserId() == null) {
            return;
        }

        // 1. 构建通知消息
        TaskNotificationMessage message = new TaskNotificationMessage();
        message.setTaskId(task.getTaskId());
        message.setTaskType(task.getTaskType());
        message.setTaskName(task.getTaskName());
        message.setStatus(task.getStatus());
        message.setProgress(task.getProgress());
        message.setErrorMessage(task.getErrorMessage());
        message.setResultFileName(task.getResultFileName());

        // 2. 发送消息给指定用户
        String destination = DESTINATION_FORMAT.formatted(task.getUserId());
        messagingTemplate.convertAndSend(destination, message);

        log.info("任务状态通知已发送: taskId={}, userId={}, status={}",
                task.getTaskId(), task.getUserId(), task.getStatus());
    }

    /**
     * 任务通知消息
     */
    @Data
    public static class TaskNotificationMessage {
        private String taskId;
        private String taskType;
        private String taskName;
        private String status;
        private Integer progress;
        private String errorMessage;
        private String resultFileName;
    }
}
