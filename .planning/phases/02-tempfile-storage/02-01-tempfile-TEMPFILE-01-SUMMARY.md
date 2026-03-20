---
phase: 02-tempfile-storage
plan: 01
subsystem: ai-chatbi
tags: [tempfile, chatbi, sql-execution]
dependency_graph:
  requires: []
  provides: [TEMPFILE-01]
  affects: [ExecuteSqlTool, ChatContext]
tech_stack:
  added:
    - TempFileManager (Spring Service)
  patterns:
    - JSON Lines 格式存储
    - 配置驱动阈值
key_files:
  created:
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/TempFileManager.java
  modified:
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/ChatContext.java
    - ai-chatbi/src/main/java/cn/opensrcdevelop/ai/chat/tool/impl/ExecuteSqlTool.java
decisions:
  - id: JSON Lines 格式
    rationale: 每行独立解析，适合大数据流式处理和随机读取
  - id: 阈值默认 100 条
    rationale: 可配置，通过 tempfile.threshold 调整
  - id: 超过阈值时 queryData 返回 null
    rationale: 避免大数据量通过 SSE 传输，保持连接稳定
metrics:
  duration: "~5 分钟"
  completed: "2026-03-20"
---

# Phase 02 Plan 01: SQL 查询临时文件存储

**一句话描述：** 实现 SQL 查询结果的临时文件存储功能，当结果超过配置阈值时写入 JSON Lines 格式的 .txt 文件

## 变更概述

为 ChatBI 模块添加临时文件存储功能。当 SQL 查询结果超过配置阈值（默认 100 条）时，将结果写入临时目录的 .txt 文件（JSON Lines 格式），避免大数据集占用内存和 SSE 连接不稳定。

## 主要变更

### 1. ChatContext 添加 tempFilePath 字段
- 新增 `private String tempFilePath` 字段
- 提供 getter/setter 用于存储临时文件路径
- 位置：`ChatContext.java`

### 2. 创建 TempFileManager 服务类
- **位置：** `TempFileManager.java`
- **核心方法：**
  - `writeQueryDataToTempFile(List<Map<String, Object>> data, String chatId)` - 检查阈值并写入临时文件
  - `getTempFilePath(String chatId)` - 获取指定会话的临时文件路径
  - `deleteTempFile(String filePath)` - 删除指定临时文件
- **配置项：**
  - `tempfile.threshold` - 阈值，默认 100
  - `tempfile.directory` - 存储目录，默认系统临时目录
- **文件命名：** `chatbi_{chatId}_{timestamp}.txt`
- **格式：** JSON Lines（每行一个 JSON 对象）

### 3. ExecuteSqlTool 集成 TempFileManager
- 注入 `TempFileManager` 依赖
- SQL 执行后检查数据条数是否超过阈值
- 超过阈值时：
  - 调用 `writeQueryDataToTempFile` 写入临时文件
  - 设置 `ChatContext.tempFilePath`
  - Response 设置 `tempFilePath` 和 `recordCount`
  - `queryData` 返回 null（避免大数据传输）
- Response 新增字段：`tempFilePath`、`recordCount`

## 验证结果

| 检查项 | 状态 |
|--------|------|
| 编译成功 | PASS |
| ChatContext 包含 tempFilePath | PASS |
| TempFileManager 方法完整 | PASS |
| ExecuteSqlTool 集成 TempFileManager | PASS |

## 提交记录

| Commit | 描述 |
|--------|------|
| `7f113f6` | feat(02-tempfile): 添加 ChatContext.tempFilePath 字段 |
| `f075e94` | feat(02-tempfile): 创建 TempFileManager 类 |
| `166bd58` | feat(02-tempfile): 集成 TempFileManager 到 ExecuteSqlTool |

## 后续计划

- Phase 02 Plan 02: 添加随机读取 Tool（read_query_result）
- 前端 AI 对话可获取 `tempFilePath` 并调用 `read_query_result` 读取数据
