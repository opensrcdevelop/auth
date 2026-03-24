# Plan 01-02: 后端 showThinking 传播链路 - 执行总结

**Plan:** 01-02
**Phase:** 01-think-process-control
**Status:** ✅ 完成

## 执行概要

完成后端 showThinking 字段的完整传播链路：
- ChatBIRequestDto → ChatContext → ThinkAnswerAgent (guard)

## 文件修改

| 文件 | 说明 |
|------|------|
| `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/ChatBIRequestDto.java` | 添加 showThinking 字段 |
| `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java` | 添加 showThinking 字段 |
| `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/agent/ThinkAnswerAgent.java` | 保护 5 处 sendChatBIThinking 调用 |
| `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java` | 传播 showThinking 到 ChatContext |

## 实现细节

### 1. ChatBIRequestDto 添加 showThinking
```java
@Schema(description = "是否显示思考过程")
private Boolean showThinking = true;
```

### 2. ChatContext 添加 showThinking
```java
/** 是否显示思考过程 */
private Boolean showThinking = true;
```

### 3. ThinkAnswerAgent 保护 5 处调用

所有 `SseUtil.sendChatBIThinking()` 调用都被以下逻辑保护：
```java
if (Boolean.TRUE.equals(ChatContextHolder.getChatContext().getShowThinking())) {
    SseUtil.sendChatBIThinking(emitter, message, isFirst);
}
```

保护的调用位置：
- Step thinking header (循环开始)
- LLM stream output (流式输出)
- Tool execution start (工具执行开始)
- Tool execution success (工具执行成功)
- Tool execution failure (工具执行失败)

### 4. ChatBIServiceImpl 传播
```java
chatContext.setShowThinking(requestDto.getShowThinking());
```

## 验证结果

- ✅ ChatBIRequestDto: `Boolean showThinking = true` 存在
- ✅ ChatContext: `Boolean showThinking = true` 存在
- ✅ ThinkAnswerAgent: 5 处 `getShowThinking()` 调用
- ✅ ChatBIServiceImpl: `chatContext.setShowThinking()` 传播存在
- ✅ 编译通过：`./gradlew :ai-chatbi:compileJava`
- ✅ Spotless 格式化通过：`./gradlew :ai-chatbi:spotlessApply`
- ✅ 测试通过：`./gradlew :ai-chatbi:test --tests ThinkAnswerAgentTest`

## 数据流

```
前端 localStorage
    ↓
Chat.vue showThinking ref
    ↓ (fetchStream body)
showThinking: showThinking.value
    ↓
ChatBIRequestDto.showThinking
    ↓ (ChatBIServiceImpl.streamChatBI)
chatContext.setShowThinking()
    ↓
ThinkAnswerAgent.thinkAnswer()
    ↓ (Boolean.TRUE.equals(ChatContextHolder.getChatContext().getShowThinking()))
SseUtil.sendChatBIThinking() [guarded]
```
