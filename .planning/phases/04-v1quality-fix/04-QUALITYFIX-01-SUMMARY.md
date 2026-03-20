---
phase: 04-v1quality-fix
plan: 01
subsystem: ai-chatbi
tags: [quality-fix, refactor, naming]

dependency_graph:
  requires: []
  provides:
    - id: QUALITYFIX-01
      description: "修复 v1.0 实现质量问题"
tech_stack:
  added: []
  modified:
    - ChatContext (queryResultFilePaths)
    - ExecuteSqlTool
    - ReadQueryResultTool
    - ChatBIServiceImpl
    - QueryResultTempFileManager (renamed from TempFileManager)
key_files:
  created:
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/QueryResultTempFileManager.java
      description: "重命名后的类，属性使用 chatbi.query-result.* 规范"
  modified:
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java
      description: "tempFilePath 改为 List<String> queryResultFilePaths"
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java
      description: "使用 QueryResultTempFileManager，ChatContext.queryData 保持不变"
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java
      description: "支持 filePath 参数指定读取路径"
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
      description: "统一清理逻辑，删除所有 temp 文件"
decisions:
  - id: "query-result-naming"
    rationale: "类名和属性名更准确地描述其功能范围"
  - id: "list-file-paths"
    rationale: "支持同一会话多次执行 SQL，存储所有 temp 文件路径"
  - id: "readerspecify-path"
    rationale: "ReadQueryResultTool 支持 AI 传入 filePath 指定读取哪个文件"
  - id: "context-data-preserved"
    rationale: "ChatContext.queryData 始终保留完整数据，不受 temp 文件存储影响"
metrics:
  duration: "~5 min"
  completed: "2026-03-21"
---

# Phase 04 Plan 01 Summary: v1.0 Quality Fix

## 概述

修复 v1.0 实现中的质量问题，包括类命名、属性命名、数据一致性和多文件清理。

## 修复的问题

| # | 问题 | 修复方案 |
|---|------|---------|
| 1 | TempFileManager 命名不够贴切 | 重命名为 QueryResultTempFileManager |
| 2 | 属性名 tempfile.* 不规范 | 改为 chatbi.query-result.* |
| 3 | tempFilePath 只存单个路径 | 改为 List<String> queryResultFilePaths |
| 4 | ExecuteSqlTool 清除 queryData | ChatContext.queryData 始终保留完整数据 |
| 5 | ReadQueryResultTool 无法指定路径 | 支持 filePath 参数指定读取路径 |
| 6 | 多次 SQL 产生的多个文件未清理 | 会话结束时清理所有 temp 文件 |
| 7 | application-ai.properties 缺少配置 | 添加 chatbi.query-result.* 配置 |

## 变更内容

### 1. QueryResultTempFileManager（新类）

**文件**: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/QueryResultTempFileManager.java`

- 从 TempFileManager 重命名
- 属性重命名：
  - `tempfile.threshold` → `chatbi.query-result.threshold`
  - `tempfile.directory` → `chatbi.query-result.directory`

### 2. ChatContext

**文件**: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java`

- 字段变更：
  ```java
  // 旧
  private String tempFilePath;

  // 新
  private List<String> queryResultFilePaths = new ArrayList<>();
  ```
- 新增方法：
  - `addQueryResultFilePath(String path)` - 添加路径
  - `clearQueryResultFilePaths()` - 清空路径列表

### 3. ExecuteSqlTool

**文件**: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java`

- 使用 QueryResultTempFileManager
- **关键修复**：不设置 `response.setQueryData(null)` 时不再影响 ChatContext.queryData
- Response 只返回 temp 文件引用（filePath、recordCount）

### 4. ReadQueryResultTool

**文件**: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java`

- 新增 filePath 参数（可选）：
  ```java
  @ToolParam(description = "The temp file path returned from execute_sql tool response (optional)", required = false)
  private String filePath;
  ```
- 优先使用请求中指定的路径，否则使用 ChatContext 中的最新路径

### 5. ChatBIServiceImpl

**文件**: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java`

- 新增 `cleanupTempFiles(ChatContext)` 方法
- 在 onCompletion、onTimeout、onError 回调中统一清理
- finally 块中执行清理

### 6. application-ai.properties

**文件**: `auth-server/src/main/resources/application-ai.properties`

新增配置：
```properties
# ChatBI 查询结果临时文件配置
chatbi.query-result.threshold=100
chatbi.query-result.directory=${java.io.tmpdir}
```

## 设计决策

1. **query-result 命名**：更准确描述功能范围
2. **List 存储多路径**：支持同一会话多次执行 SQL
3. **AI 指定读取路径**：ReadQueryResultTool 支持 filePath 参数
4. **queryData 保持不变**：ChatContext.queryData 始终保留完整数据

## 验证结果

| 检查项 | 结果 |
|--------|------|
| 编译通过 | BUILD SUCCESSFUL |
| TempFileManager 已重命名 | 已验证 |
| 所有属性使用 chatbi.query-result.* | 已验证 |
| ChatContext.queryData 保留完整数据 | 已验证 |
| ReadQueryResultTool 支持 filePath | 已验证 |
| 所有 temp 文件统一清理 | 已验证 |

## Self-Check: PASSED

- [x] QueryResultTempFileManager.java 已创建
- [x] TempFileManager.java 已删除
- [x] ChatContext 已更新
- [x] ExecuteSqlTool 已更新
- [x] ReadQueryResultTool 已更新
- [x] ChatBIServiceImpl 已更新
- [x] application-ai.properties 已添加配置
- [x] 编译通过

---
*Phase: 04-v1quality-fix*
*Completed: 2026-03-21*
