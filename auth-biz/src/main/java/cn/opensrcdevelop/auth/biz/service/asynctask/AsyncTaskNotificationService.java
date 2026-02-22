package cn.opensrcdevelop.auth.biz.service.asynctask;

import cn.opensrcdevelop.auth.biz.entity.asynctask.AsyncTask;
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

        // 构建通知消息
        TaskNotificationMessage message = new TaskNotificationMessage();
        message.setTaskId(task.getTaskId());
        message.setTaskType(task.getTaskType());
        message.setTaskName(task.getTaskName());
        message.setStatus(task.getStatus());
        message.setProgress(task.getProgress());
        message.setErrorMessage(task.getErrorMessage());
        message.setResultFileName(task.getResultFileName());

        // 发送消息给指定用户
        String destination = "/queue/user/" + task.getUserId() + "/tasks";
        messagingTemplate.convertAndSend(destination, message);

        log.info("任务状态通知已发送: taskId={}, userId={}, status={}",
                task.getTaskId(), task.getUserId(), task.getStatus());
    }

    /**
     * 通知用户任务进度更新
     *
     * @param taskId
     *            任务ID
     * @param userId
     *            用户ID
     * @param taskType
     *            任务类型
     * @param status
     *            任务状态
     * @param progress
     *            进度
     */
    public void notifyTaskProgress(String taskId, String userId, String taskType, String status, Integer progress) {
        TaskNotificationMessage message = new TaskNotificationMessage();
        message.setTaskId(taskId);
        message.setTaskType(taskType);
        message.setStatus(status);
        message.setProgress(progress);

        String destination = "/queue/user/" + userId + "/tasks";
        messagingTemplate.convertAndSend(destination, message);
    }

    /**
     * 任务通知消息
     */
    @lombok.Data
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
