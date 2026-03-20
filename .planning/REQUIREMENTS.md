# Requirements: ChatBI 连接增强

**Defined:** 2026-03-20
**Core Value:** 保持 SSE 连接稳定，避免大数据集导致的连接断开，同时让 AI 能够处理超出内存限制的查询结果。

## v1 Requirements

### SSE 心跳

- [ ] **HEARTBEAT-01**: SSE 连接每 10 秒自动发送空消息心跳，保持连接不断开

### 临时文件存储

- [ ] **TEMPFILE-01**: SQL 查询结果超过配置阈值时，每条数据 JSON 化存储为一行 txt 文件
- [ ] **TEMPFILE-02**: 临时文件在单轮会话结束时自动删除

### 随机读取 Tool

- [ ] **READERTOOL-01**: 创建 `read_query_result` Tool，支持 AI 传入 offset 和 limit 按偏移量读取临时文件

## Out of Scope

| Feature | Reason |
|---------|--------|
| 修改 SQL 执行逻辑 | 仅改结果返回方式，不改查询本身 |
| 永久保存查询结果 | 临时文件单轮会话结束即删除 |
| 多文件拆分策略 | 单文件存储，按偏移量读取即可满足需求 |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| HEARTBEAT-01 | — | Pending |
| TEMPFILE-01 | — | Pending |
| TEMPFILE-02 | — | Pending |
| READERTOOL-01 | — | Pending |

**Coverage:**
- v1 requirements: 4 total
- Mapped to phases: 0
- Unmapped: 4 ⚠️

---
*Requirements defined: 2026-03-20*
*Last updated: 2026-03-20 after initial definition*
