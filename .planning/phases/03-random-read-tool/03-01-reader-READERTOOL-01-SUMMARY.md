---
phase: "03-random-reader"
plan: "01"
subsystem: ai-chatbi
tags: [read-query-result, temp-file, random-read, tool]
dependency_graph:
  requires: []
  provides:
    - id: READERTOOL-01
      description: "AI can call read_query_result Tool with offset and limit parameters"
  affects: []
tech_stack:
  added:
    - Java IO (BufferedReader, FileReader)
    - Jackson ObjectMapper for JSON parsing
  patterns:
    - MethodTool interface implementation
    - JSON Lines format for random access
key_files:
  created:
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java
      description: "Tool 实现，支持按 offset/limit 分页读取临时文件"
  modified:
    - path: ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java
      description: "新增 readLinesFromTempFile 方法"
decisions:
  - id: "JSON-Lines-random-access"
    rationale: "每行独立 JSON，便于按行随机读取，无需加载整个文件到内存"
  - id: "offset-limit-pagination"
    rationale: "使用 offset/limit 分页，AI 可多次调用获取完整数据"
metrics:
  duration: "~3 min"
  completed: "2026-03-20T13:15:00Z"
---

# Phase 03 Plan 01 Summary: read_query_result Tool

## 概述

创建 `read_query_result` Tool，支持 AI 按偏移量随机读取临时文件中的查询结果，解决大数据量场景下的内存限制问题。

## 实现的 Truths

| Truth | 状态 |
|-------|------|
| AI can call read_query_result Tool with offset and limit parameters | 已实现 |
| Tool returns JSON Lines data from the specified offset position | 已实现 |
| Tool returns error when temp file does not exist | 已实现 |

## 变更内容

### 1. TempFileManager 新增方法

**文件**: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java`

新增 `readLinesFromTempFile(String filePath, int offset, int limit)` 方法：

- 使用 `BufferedReader` 按行读取文件
- 跳过前 `offset` 行
- 最多读取 `limit` 行
- 每行作为独立 JSON 解析后装入 `List<Map<String, Object>>` 返回
- 文件不存在时返回 `null`
- 使用 try-with-resources 保证资源关闭

### 2. 新增 ReadQueryResultTool

**文件**: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ReadQueryResultTool.java`

- Tool 名称: `read_query_result`
- 实现 `MethodTool` 接口
- 从 `ChatContextHolder` 获取当前会话的 `tempFilePath`
- 调用 `TempFileManager.readLinesFromTempFile(path, offset, limit)` 获取数据
- 返回 `Response`（包含 `queryData`、`recordCount`、`hasMore`、`error`）

**Request 参数**:
- `offset`: int - 起始偏移量
- `limit`: int - 要读取的条数（推荐 100-500）

**Response 字段**:
- `queryData`: List<Map<String, Object>> - 读取的数据
- `recordCount`: int - 本次返回的条数
- `hasMore`: boolean - 是否还有更多数据
- `error`: string - 错误信息（如有）

## 验证结果

| 检查项 | 结果 |
|--------|------|
| 编译通过 | BUILD SUCCESSFUL |
| ReadQueryResultTool 类存在 | 已验证 |
| 实现 MethodTool 接口 | 已验证 |
| Tool 名称为 read_query_result | 已验证 |
| readLinesFromTempFile 方法存在 | 已验证 |

## Deviations from Plan

无偏差 - 计划执行完全符合预期。

## Commits

- `0b0a969`: feat(03-random-read-tool): 添加 read_query_result Tool 实现随机读取

## Self-Check: PASSED

- [x] ReadQueryResultTool.java 存在
- [x] TempFileManager.java 已更新
- [x] 编译通过
- [x] 提交完成
