---
phase: 01-sse-heartbeat
plan: 01
type: execute
wave: 1
depends_on: []
files_modified:
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/util/SseUtil.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/HeartbeatManager.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
  - auth-server/src/main/java/cn/opensrcdevelop/auth/config/ExecutorConfig.java
autonomous: true
requirements:
  - HEARTBEAT-01
gap_closure: false

must_haves:
  truths:
    - "SSE connection establishes heartbeat that fires every 10 seconds"
    - "Heartbeat continues for the entire SSE connection lifecycle"
    - "Heartbeat stops and releases resources when SSE connection closes"
  artifacts:
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/util/SseUtil.java"
      contains: "sendHeartbeat"
      min_lines: 10
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/HeartbeatManager.java"
      provides: "Heartbeat scheduling and cancellation"
      min_lines: 40
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java"
      contains: "heartbeatManager"
      min_lines: 5
  key_links:
    - from: "ChatBIServiceImpl.streamChatBI()"
      to: "HeartbeatManager"
      via: "startHeartbeat() call at connection start"
    - from: "ChatBIServiceImpl"
      to: "HeartbeatManager"
      via: "cancelHeartbeat() calls in onCompletion/onTimeout/onError callbacks"
    - from: "HeartbeatManager"
      to: "SseUtil.sendHeartbeat()"
      via: "scheduled task invokes sendHeartbeat every 10 seconds"
---

<objective>
实现 SSE 心跳机制，保持连接稳定不断开。

**目的**: 每 10 秒自动发送空消息心跳，防止连接因空闲被关闭
**产出**:
- HeartbeatManager 类 - 管理心跳调度
- SseUtil.sendHeartbeat() - 发送心跳空消息
- ChatBIServiceImpl 集成心跳机制
</objective>

<execution_context>
@/Users/lee0407/dev/projs/auth/worktrees/feature-chatbi-heartbeat-tempfile/.claude/get-shit-done/workflows/execute-plan.md
</execution_context>

<context>
@ai-chatbi/src/main/java/cn/opensrcdevelop/ai/util/SseUtil.java
@ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
@common/src/main/java/cn/opensrcdevelop/common/constants/ExecutorConstants.java
@auth-server/src/main/java/cn/opensrcdevelop/auth/config/ExecutorConfig.java
</context>

<tasks>

<task type="auto">
  <name>Task 1: 添加 SseUtil.sendHeartbeat() 心跳方法</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/util/SseUtil.java</files>
  <read_first>
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/util/SseUtil.java (现有的 send 方法模式)
  </read_first>
  <action>
在 SseUtil.java 中添加 sendHeartbeat 方法。

参考现有方法的 Try.run() 模式添加以下方法:

```java
/**
 * 发送 SSE 心跳（空消息）
 *
 * @param emitter SseEmitter
 */
public static void sendHeartbeat(SseEmitter emitter) {
    Try.run(() -> emitter.send(SseEmitter.event().data("")));
}
```

将新方法添加到类末尾（在最后一个右括号之前），遵循现有代码风格。
  </action>
  <verify>
    <automated>grep -c "sendHeartbeat" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/util/SseUtil.java</automated>
  </verify>
  <acceptance_criteria>
    - SseUtil.java 包含 `public static void sendHeartbeat(SseEmitter emitter)` 方法
    - 方法使用 `Try.run()` 包裹，与其他 send 方法一致
    - 方法使用 `emitter.send(SseEmitter.event().data(""))` 发送空数据心跳
  </acceptance_criteria>
  <done>SseUtil.sendHeartbeat() 方法已添加，发送空字符串数据作为心跳</done>
</task>

<task type="auto">
  <name>Task 2: 创建 HeartbeatManager 类</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/HeartbeatManager.java</files>
  <read_first>
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java (了解 executor 注入方式)
  </read_first>
  <action>
在 ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ 目录下创建 HeartbeatManager.java

```java
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
```
  </action>
  <verify>
    <automated>test -f ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/HeartbeatManager.java && echo "EXISTS"</automated>
  </verify>
  <acceptance_criteria>
    - HeartbeatManager.java 文件已创建
    - 类使用 @Component 注解（Spring Bean）
    - 包含 startHeartbeat(SseEmitter) 方法返回 ScheduledFuture
    - 包含 stopHeartbeat(ScheduledFuture) 方法
    - 心跳间隔常量 HEARTBEAT_INTERVAL_SECONDS = 10
    - 使用 ScheduledExecutorService.scheduleAtFixedRate 实现调度
  </acceptance_criteria>
  <done>HeartbeatManager 类已创建，包含 startHeartbeat 和 stopHeartbeat 方法</done>
</task>

<task type="auto">
  <name>Task 3: 在 ChatBIServiceImpl 中集成心跳机制</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java</files>
  <read_first>
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
  </read_first>
  <action>
修改 ChatBIServiceImpl.java 实现心跳集成:

1. 添加 HeartbeatManager 依赖注入（在类成员变量区域）:
```java
private final HeartbeatManager heartbeatManager;
```

2. 在构造器或字段中添加（跟随现有 final 字段模式）

3. 在 streamChatBI() 方法中，创建 emitter 后启动心跳:
```java
SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);
AtomicBoolean interruptFlag = new AtomicBoolean(false);
SecurityContext securityContext = SecurityContextHolder.getContext();

ScheduledFuture<?> heartbeatFuture = heartbeatManager.startHeartbeat(emitter);
```

4. 在 emitter 的回调中停止心跳:
```java
emitter.onCompletion(() -> {
    log.info("ChatBI 对话（{}）中断/结束", finalChatId);
    interruptFlag.set(true);
    heartbeatManager.stopHeartbeat(heartbeatFuture);
});

emitter.onTimeout(() -> {
    log.info("ChatBI 对话（{}）超时", finalChatId);
    interruptFlag.set(true);
    heartbeatManager.stopHeartbeat(heartbeatFuture);
});

emitter.onError(e -> {
    log.info("ChatBI 对话（{}）异常: {}", finalChatId, e.getMessage());
    interruptFlag.set(true);
    heartbeatManager.stopHeartbeat(heartbeatFuture);
});
```

注意: 当前代码中只有 `emitter.onCompletion()`，需要添加 `onTimeout` 和 `onError` 回调。

修改点总结:
- 添加 `private final HeartbeatManager heartbeatManager;` 字段
- 在 `executor.execute(() -> {` 之前调用 `heartbeatManager.startHeartbeat(emitter)` 并保存返回值
- 扩展 `emitter.onCompletion()` 回调添加 stopHeartbeat 调用
- 添加 `emitter.onTimeout()` 回调，包含 stopHeartbeat 调用
- 添加 `emitter.onError(e -> {})` 回调，包含 stopHeartbeat 调用
  </action>
  <verify>
    <automated>grep -c "heartbeatManager" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java</automated>
  </verify>
  <acceptance_criteria>
    - ChatBIServiceImpl.java 包含 `private final HeartbeatManager heartbeatManager;`
    - streamChatBI() 方法中调用 `heartbeatManager.startHeartbeat(emitter)`
    - emitter.onCompletion() 回调中调用 `heartbeatManager.stopHeartbeat(heartbeatFuture)`
    - 新增 `emitter.onTimeout(() -> {...})` 回调包含 stopHeartbeat
    - 新增 `emitter.onError(e -> {...})` 回调包含 stopHeartbeat
    - ScheduledFuture 变量在方法内声明，用于管理心跳生命周期
  </acceptance_criteria>
  <done>ChatBIServiceImpl 已集成心跳机制，SSE 连接建立后自动启动心跳，连接关闭/超时/异常时停止心跳</done>
</task>

</tasks>

<verification>
- SseUtil.sendHeartbeat() 方法存在且格式正确
- HeartbeatManager 类存在且包含 startHeartbeat/stopHeartbeat
- ChatBIServiceImpl 包含 heartbeatManager 字段和所有回调处理
- 代码编译通过: ./gradlew :ai-chatbi:compileJava :auth-server:compileJava -x test
</verification>

<success_criteria>
- HEARTBEAT-01: SSE 连接每 10 秒自动发送空消息心跳
- 心跳在连接生命周期内持续发送
- 连接关闭时心跳正确停止，不产生资源泄漏
</success_criteria>

<output>
After completion, create `.planning/phases/01-sse-heartbeat/01-heartbeat-HEARTBEAT-01-SUMMARY.md`
</output>
