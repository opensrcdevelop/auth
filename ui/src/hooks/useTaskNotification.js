import { onMounted, onUnmounted, ref } from "vue";
import { Client } from "@stomp/stompjs";
import SockJS from "sockjs-client";
import { Notification } from "@arco-design/web-vue";
import { getOAuthIssuer, getSubDomain } from "@/util/tool";
import { taskEmitter } from "./taskEmitter";
import { AUTH_TOKENS } from "@/util/constants";
/**
 * 任务通知 WebSocket Hook
 */
export var useTaskNotification = function (onMessage) {
    var connected = ref(false);
    var stompClient = null;
    var connect = function () {
        // 获取基础 URL（SockJS 需要 HTTP/HTTPS URL，不是 ws/wss）
        var baseUrl = getOAuthIssuer();
        var wsUrl = "".concat(baseUrl, "/ws");
        // 获取认证 Token
        var authTokens = localStorage.getItem(AUTH_TOKENS);
        var authHeader = "";
        if (authTokens) {
            try {
                var token = JSON.parse(atob(authTokens));
                if (token && token.access_token) {
                    authHeader = "".concat(token.token_type || "Bearer", " ").concat(token.access_token);
                }
            }
            catch (e) {
                console.error("解析 token 失败", e);
            }
        }
        stompClient = new Client({
            webSocketFactory: function () { return new SockJS(wsUrl); },
            reconnectDelay: 5000,
            heartbeatIncoming: 4000,
            heartbeatOutgoing: 4000,
            onConnect: function () {
                connected.value = true;
                // 订阅用户任务通知队列
                var authTokens = localStorage.getItem(AUTH_TOKENS);
                var userId = "anonymous";
                if (authTokens) {
                    try {
                        var tokenData = JSON.parse(atob(authTokens));
                        var accessToken = tokenData.access_token;
                        if (accessToken) {
                            var parts = accessToken.split(".");
                            var payloadStr = parts[1].replace(/-/g, "+").replace(/_/g, "/");
                            var padding = "=".repeat((4 - payloadStr.length % 4) % 4);
                            var paddedPayload = payloadStr + padding;
                            var payload = JSON.parse(atob(paddedPayload));
                            userId = payload.sub || "anonymous";
                        }
                    }
                    catch (e) {
                        console.error("解析 token 失败", e);
                    }
                }
                // 获取租户编码（空字符串表示默认租户）
                var tenantCode = getSubDomain();
                var destination = tenantCode
                    ? "/queue/".concat(tenantCode, "/user/").concat(userId, "/tasks")
                    : "/queue/user/".concat(userId, "/tasks");
                // 订阅用户任务通知队列
                stompClient === null || stompClient === void 0 ? void 0 : stompClient.subscribe(destination, function (message) {
                    try {
                        var body = JSON.parse(message.body);
                        // 显示通知
                        var statusText = getStatusText(body.status);
                        Notification.info({
                            title: "任务状态更新",
                            content: "".concat(body.taskName || body.taskType, " - ").concat(statusText),
                            duration: 3000,
                        });
                        // 发送全局事件
                        taskEmitter.emit("task:update", body);
                        // 回调处理
                        if (onMessage) {
                            onMessage(body);
                        }
                    }
                    catch (e) {
                        console.error("解析任务通知消息失败", e);
                    }
                });
            },
            onDisconnect: function () {
                connected.value = false;
            },
            onStompError: function (frame) {
                console.error("STOMP 错误", frame);
            },
        });
        stompClient.activate();
    };
    var disconnect = function () {
        if (stompClient) {
            stompClient.deactivate();
            stompClient = null;
        }
    };
    var getStatusText = function (status) {
        var map = {
            PENDING: "等待中",
            RUNNING: "执行中",
            SUCCESS: "完成",
            FAILED: "失败",
            CANCELLED: "已取消",
        };
        return status ? map[status] || status : "未知";
    };
    onMounted(function () {
        connect();
    });
    onUnmounted(function () {
        disconnect();
    });
    return {
        connected: connected,
        connect: connect,
        disconnect: disconnect,
    };
};
