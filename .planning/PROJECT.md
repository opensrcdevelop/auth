# ChatBI 连接增强

## What This Is

对现有 chatbi 模块进行三项增强：
1. **SSE 心跳包**：每 10 秒发送空消息保持连接不断开
2. **大结果集临时文件存储**：SQL 查询结果超过阈值时存入 JSON Lines 格式的临时文件
3. **随机读取 Tool**：AI 可按偏移量从临时文件读取指定条数的数据继续分析

## Core Value

保持 SSE 连接稳定，避免大数据集查询导致的连接断开，同时让 AI 能够处理超出内存限制的查询结果。

## Requirements

### Validated

- ✓ `ChatBIController.streamChatBI()` 提供 SSE 接口，超时 60 分钟 — 现有
- ✓ `ExecuteSqlTool` 执行 SQL 并返回 `List<Map<String, Object>>` — 现有
- ✓ `SseUtil` 提供多种消息发送方法 — 现有
- ✓ Tool 通过 `MethodTool` 接口实现，`@Component` 注册 — 现有
- ✓ `ChatContextHolder` 管理会话上下文 — 现有
- ✓ **HEARTBEAT-01**: SSE 连接每 10 秒自动发送空消息心跳，保持连接不断开 — Phase 1 完成
- ✓ **TEMPFILE-01**: SQL 查询结果超过配置阈值时，每条数据 JSON 化存储为一行 txt 文件 — Phase 2 完成
- ✓ **TEMPFILE-02**: 临时文件在单轮会话结束时自动删除 — Phase 2 完成

### Active

- [ ] **READERTOOL-01**: 创建 `read_query_result` Tool，支持 AI 传入 offset 和 limit 按偏移量读取临时文件

### Out of Scope

- 修改 SQL 执行逻辑（仅改结果返回方式）
- 永久保存查询结果
- 多文件拆分策略

## Context

现有 ai-chatbi 模块使用 Spring AI 通过 `MethodTool` 接口模式暴露 Tool。`ExecuteSqlTool` 直接返回完整查询结果，大数据集会占用大量内存并可能导致 SSE 连接不稳定。

关键现有文件：
- `ChatBIServiceImpl.streamChatBI()` — SSE 连接入口
- `SseUtil` — SSE 消息发送工具类
- `ExecuteSqlTool` — SQL 执行 Tool，结果直接返回 `List<Map<String, Object>>`
- `ChatContextHolder` — 会话上下文管理
- `ThinkAnswerAgent` — Tool 调用执行代理

## Constraints

- **心跳频率**：10 秒/次，空消息内容，前端可忽略
- **临时文件阈值**：可配置，默认 100 条
- **临时文件格式**：JSON Lines（每行一个 JSON 对象），`.txt` 后缀
- **临时文件删除**：单轮会话结束（`SseEmitter.onCompletion()` 或超时）时删除
- **读取方式**：按偏移量读取，由 AI 通过 Tool 参数指定 offset 和要读取的条数

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| 临时文件用 JSON Lines 格式 | 每行独立解析，适合随机读取 | — Pending |
| Tool 命名 `read_query_result` | 与 `execute_sql` 对应，语义清晰 | — Pending |
| 单轮会话结束删除文件 | 避免占用磁盘空间 | — Pending |

---
*Last updated: 2026-03-20 after Phase 2*
