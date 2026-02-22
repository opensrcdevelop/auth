// 全局任务通知事件发送器
import mitt, {Emitter} from "mitt";
import type {TaskNotificationMessage} from "./useTaskNotification";

export type TaskEvents = {
  "task:update": TaskNotificationMessage;
};

export const taskEmitter: Emitter<TaskEvents> = mitt<TaskEvents>();
