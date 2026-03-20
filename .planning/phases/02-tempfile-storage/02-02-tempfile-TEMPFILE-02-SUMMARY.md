---
phase: 02-tempfile-storage
plan: 02
type: summary
subsystem: ai-chatbi
tags: [tempfile, SSE, cleanup]
dependency_graph:
  requires:
    - "02-01-tempfile-TEMPFILE-01"
  provides:
    - "TEMPFILE-02: SSE 会话结束时自动删除临时文件"
  affects:
    - "ChatBIServiceImpl"
tech_stack:
  added:
    - "AtomicReference<String> for lambda-captured tempFilePath"
  patterns:
    - "SSE callback cleanup"
    - "finally block fallback cleanup"
key_files:
  created: []
  modified:
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
      description: 注入 TempFileManager，在 SSE 回调和 finally 块中清理临时文件
decisions:
  - id: tempfile-cleanup-scoped-to-callbacks
    decision: "使用 AtomicReference<String> 解决 lambda 捕获变量问题"
    rationale: "tempFilePath 在 finally 块中才被赋值，需要能被 lambda 访问"
  - id: tempfile-double-cleanup
    decision: "在回调和 finally 块中都执行清理，形成双重保障"
    rationale: "回调确保及时清理，finally 块兜底防止遗漏"
metrics:
  duration: "~10 minutes"
  completed: "2026-03-20"
---

# Phase 02 Plan 02 Summary: SSE 会话结束时自动删除临时文件

## 一句话说明
在 SSE 会话结束时（正常完成、超时、异常）自动删除临时文件，确保单轮会话结束即清理。

## 任务执行

### Task 1: 注入 TempFileManager 到 ChatBIServiceImpl
- **状态**: 完成
- **变更**:
  - 添加 `private final TempFileManager tempFileManager;` 字段
  - 添加 `AtomicReference<String> tempFilePathRef` 用于在 lambda 中捕获 tempFilePath
  - 在 finally 块中添加兜底清理逻辑

### Task 2: 在 onCompletion/onTimeout/onError 回调中确保清理
- **状态**: 完成
- **变更**: 在所有三个 SSE 回调中添加 `tempFileManager.deleteTempFile(path)` 调用
  - `emitter.onCompletion()`: 正常中断/结束时清理
  - `emitter.onTimeout()`: 超时时清理
  - `emitter.onError()`: 异常时清理

## 验证结果

| 验证项 | 结果 |
|--------|------|
| 编译成功 | `./gradlew :ai-chatbi:compileJava` 通过 |
| Spotless 格式化 | `./gradlew spotlessApply` 通过 |
| deleteTempFile 调用数 | 4 处（3 回调 + 1 finally 块） |

## 关键代码

```java
// 回调中清理
emitter.onCompletion(() -> {
    log.info("ChatBI 对话（{}）中断/结束", finalChatId);
    interruptFlag.set(true);
    heartbeatManager.stopHeartbeat(heartbeatFuture);
    String path = tempFilePathRef.get();
    if (StringUtils.isNotBlank(path)) {
        try {
            tempFileManager.deleteTempFile(path);
        } catch (Exception e) {
            log.warn("清理临时文件失败: {}", path, e);
        }
    }
});

// finally 块兜底清理
finally {
    if (chatContext != null) {
        tempFilePathRef.set(chatContext.getTempFilePath());
    }
    emitter.complete();
    ChatContextHolder.removeChatContext(finalChatId);
    String path = tempFilePathRef.get();
    if (StringUtils.isNotBlank(path)) {
        try {
            tempFileManager.deleteTempFile(path);
        } catch (Exception e) {
            log.warn("清理临时文件失败: {}", path, e);
        }
    }
}
```

## Deviations from Plan

无偏差 - 计划完全按预期执行。

## Commit

```
17fa75a feat(02-tempfile-storage): 在 SSE 会话结束时自动删除临时文件
```

## Self-Check

- [x] 编译成功
- [x] 格式化通过
- [x] deleteTempFile 在所有回调和 finally 中调用
- [x] 使用 AtomicReference 解决 lambda 捕获问题
- [x] 任务已提交
