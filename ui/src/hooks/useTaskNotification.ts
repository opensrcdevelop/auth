import {onMounted, onUnmounted, ref} from "vue";
import {Client} from "@stomp/stompjs";
import SockJS from "sockjs-client";
import {Notification} from "@arco-design/web-vue";
import {getOAuthIssuer, getSubDomain} from "@/util/tool";
import {taskEmitter} from "./taskEmitter";
import {AUTH_TOKENS} from "@/util/constants";

/**
 * 任务通知消息类型
 */
export interface TaskNotificationMessage {
  taskId: string;
  taskType?: string;
  taskName?: string;
  status?: string;
  progress?: number;
  errorMessage?: string;
  resultFileName?: string;
}

/**
 * 任务通知 WebSocket Hook
 */
export const useTaskNotification = (onMessage?: (message: TaskNotificationMessage) => void) => {
  const connected = ref(false);
  let stompClient: Client | null = null;

  const connect = () => {
    // 获取基础 URL（SockJS 需要 HTTP/HTTPS URL，不是 ws/wss）
    const baseUrl = getOAuthIssuer();
    const wsUrl = `${baseUrl}/ws`;

    // 获取认证 Token
    const authTokens = localStorage.getItem(AUTH_TOKENS);
    let authHeader = "";
    if (authTokens) {
      try {
        const token: any = JSON.parse(atob(authTokens));
        if (token && token.access_token) {
          authHeader = `${token.token_type || "Bearer"} ${token.access_token}`;
        }
      } catch (e) {
        console.error("解析 token 失败", e);
      }
    }

    stompClient = new Client({
      webSocketFactory: () => new SockJS(wsUrl),
      reconnectDelay: 5000,
      heartbeatIncoming: 4000,
      heartbeatOutgoing: 4000,
      onConnect: () => {
        connected.value = true;

        // 订阅用户任务通知队列
        const authTokens = localStorage.getItem(AUTH_TOKENS);
        let userId = "anonymous";
        if (authTokens) {
          try {
            const tokenData = JSON.parse(atob(authTokens));
            const accessToken = tokenData.access_token;
            if (accessToken) {
              const parts = accessToken.split(".");
              const payloadStr = parts[1].replace(/-/g, "+").replace(/_/g, "/");
              const padding = "=".repeat((4 - payloadStr.length % 4) % 4);
              const paddedPayload = payloadStr + padding;
              const payload = JSON.parse(atob(paddedPayload));
              userId = payload.sub || "anonymous";
            }
          } catch (e) {
            console.error("解析 token 失败", e);
          }
        }

        // 获取租户编码（空字符串表示默认租户）
        const tenantCode = getSubDomain();
        const destination = tenantCode
            ? `/queue/${tenantCode}/user/${userId}/tasks`
            : `/queue/user/${userId}/tasks`;

        // 订阅用户任务通知队列
        stompClient?.subscribe(destination, (message) => {
          try {
            const body: TaskNotificationMessage = JSON.parse(message.body);

            // 显示通知
            const statusText = getStatusText(body.status);
            Notification.info({
              title: "任务状态更新",
              content: `${body.taskName || body.taskType} - ${statusText}`,
              duration: 3000,
            });

            // 发送全局事件
            taskEmitter.emit("task:update", body);

            // 回调处理
            if (onMessage) {
              onMessage(body);
            }
          } catch (e) {
            console.error("解析任务通知消息失败", e);
          }
        });
      },
      onDisconnect: () => {
        connected.value = false;
      },
      onStompError: (frame) => {
        console.error("STOMP 错误", frame);
      },
    });

    stompClient.activate();
  };

  const disconnect = () => {
    if (stompClient) {
      stompClient.deactivate();
      stompClient = null;
    }
  };

  const getStatusText = (status?: string): string => {
    const map: Record<string, string> = {
      PENDING: "等待中",
      RUNNING: "执行中",
      SUCCESS: "完成",
      FAILED: "失败",
      CANCELLED: "已取消",
    };
    return status ? map[status] || status : "未知";
  };

  onMounted(() => {
    connect();
  });

  onUnmounted(() => {
    disconnect();
  });

  return {
    connected,
    connect,
    disconnect,
  };
};
