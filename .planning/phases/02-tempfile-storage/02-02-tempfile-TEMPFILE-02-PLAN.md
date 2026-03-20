---
phase: 02-tempfile-storage
plan: 02
type: execute
wave: 2
depends_on: ["02-01-tempfile-TEMPFILE-01"]
files_modified:
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
  - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java
autonomous: true
requirements: [TEMPFILE-02]
must_haves:
  truths:
    - "单轮会话结束时（正常完成、超时、异常），临时文件被自动删除"
    - "删除操作在 SSE 连接关闭的各个回调中都正确执行"
  artifacts:
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java"
      provides: "会话结束时的临时文件清理逻辑"
      contains: "TempFileManager"
    - path: "ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java"
      provides: "临时文件删除方法"
      contains: "deleteTempFile"
  key_links:
    - from: "ChatBIServiceImpl.onCompletion()"
      to: "TempFileManager.deleteTempFile()"
      via: "调用临时文件删除"
      pattern: "deleteTempFile.*tempFilePath"
    - from: "ChatBIServiceImpl.onTimeout()"
      to: "TempFileManager.deleteTempFile()"
      via: "调用临时文件删除"
      pattern: "deleteTempFile.*tempFilePath"
    - from: "ChatBIServiceImpl.onError()"
      to: "TempFileManager.deleteTempFile()"
      via: "调用临时文件删除"
      pattern: "deleteTempFile.*tempFilePath"
---

<objective>
在 SSE 会话结束时自动删除临时文件，确保单轮会话结束即清理。

Purpose: 避免临时文件占用磁盘空间
Output: ChatBIServiceImpl 集成的临时文件清理逻辑
</objective>

<context>
@ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
@ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java
@ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java
</context>

<tasks>

<task type="auto">
  <name>Task 1: 注入 TempFileManager 到 ChatBIServiceImpl</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java</files>
  <action>
    在 ChatBIServiceImpl 中注入 TempFileManager：

    1. 添加构造器参数：`private final TempFileManager tempFileManager;`

    2. 在 streamChatBI() 方法的 finally 块中添加清理逻辑：
       - 通过 ChatContextHolder.getChatContext() 获取 ChatContext
       - 检查 ChatContext.tempFilePath 是否为 null/empty
       - 如果不为空，调用 tempFileManager.deleteTempFile(tempFilePath)

    注意：此清理逻辑应该在 finally 块中执行，确保无论何种结束方式都会清理
  </action>
  <verify>
    <automated>grep -n "TempFileManager\|deleteTempFile" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java</automated>
  </verify>
  <done>ChatBIServiceImpl 注入 TempFileManager 并在 finally 块中清理临时文件</done>
</task>

<task type="auto">
  <name>Task 2: 在 onCompletion/onTimeout/onError 回调中确保清理</name>
  <files>ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java</files>
  <action>
    虽然 finally 块已经会执行，但为了确保资源释放的及时性：

    1. 在 emitter.onCompletion() 回调中（132-136 行）：
       - 在调用 heartbeatManager.stopHeartbeat() 后
       - 添加 tempFileManager.deleteTempFile() 调用

    2. 在 emitter.onTimeout() 回调中（138-142 行）：
       - 在调用 heartbeatManager.stopHeartbeat() 后
       - 添加 tempFileManager.deleteTempFile() 调用

    3. 在 emitter.onError() 回调中（144-148 行）：
       - 在调用 heartbeatManager.stopHeartbeat() 后
       - 添加 tempFileManager.deleteTempFile() 调用

    注意：从 ChatContext 获取 tempFilePath，如果为空则不删除
    注意：删除操作应该使用 try-catch 包装，避免删除失败影响其他清理逻辑
  </action>
  <verify>
    <automated>grep -B2 -A2 "deleteTempFile" ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java | head -20</automated>
  </verify>
  <done>所有 SSE 回调（onCompletion/onTimeout/onError）都正确删除临时文件</done>
</task>

</tasks>

<verification>
1. 编译成功：`./gradlew :ai-chatbi:compileJava`
2. 所有 SSE 回调中都包含 tempFileManager.deleteTempFile() 调用
3. finally 块中也包含清理逻辑（兜底）
</verification>

<success_criteria>
- SSE 会话正常完成时，临时文件被删除
- SSE 会话超时时，临时文件被删除
- SSE 会话异常时，临时文件被删除
- 临时文件不存在时，删除操作不报错
</success_criteria>

<output>
创建 `.planning/phases/02-tempfile-storage/02-02-tempfile-TEMPFILE-02-SUMMARY.md`
</output>
