# ROADMAP: ChatBI 连接增强

## Overview

| Field | Value |
|-------|-------|
| Project | ChatBI 连接增强 |
| Core Value | 保持 SSE 连接稳定，避免大数据集导致的连接断开，同时让 AI 能够处理超出内存限制的查询结果。 |
| Granularity | coarse |
| Total Phases | 3 |
| Total Requirements | 4 |

## Phases

- [x] **Phase 1: SSE 心跳** - 实现每 10 秒发送空消息心跳，保持连接不断开 (completed 2026-03-20)
- [ ] **Phase 2: 临时文件存储** - SQL 查询结果超过阈值时存储为 JSON Lines 临时文件，会话结束时删除
- [ ] **Phase 3: 随机读取 Tool** - 创建 read_query_result Tool，支持按偏移量读取临时文件

## Phase Details

### Phase 1: SSE 心跳

**Goal**: SSE 连接保持稳定不断开

**Depends on**: Nothing

**Requirements**: HEARTBEAT-01

**Success Criteria** (what must be TRUE):
1. SSE 连接建立后，每 10 秒自动发送空消息内容的心跳
2. 心跳在 SSE 连接整个生命周期内持续发送，直到连接关闭
3. SSE 连接关闭时，心跳调度正确停止，不产生资源泄漏

**Plans**:
- [ ] 01-heartbeat-HEARTBEAT-01-PLAN.md - SSE 心跳实现

---

### Phase 2: 临时文件存储

**Goal**: SQL 查询结果超过阈值时存入临时文件，会话结束时自动清理

**Depends on**: Phase 1

**Requirements**: TEMPFILE-01, TEMPFILE-02

**Success Criteria** (what must be TRUE):
1. SQL 查询结果条数超过配置阈值时，结果写入临时目录的 .txt 文件（JSON Lines 格式）
2. 临时文件每行存储一个 JSON 对象，字段为 SQL 查询结果的列名和值
3. 单轮会话结束时（正常完成或超时），临时文件被自动删除
4. 临时文件存储路径可配置

**Plans**: TBD

---

### Phase 3: 随机读取 Tool

**Goal**: AI 可以通过 Tool 按偏移量读取临时文件中的查询结果

**Depends on**: Phase 2

**Requirements**: READERTOOL-01

**Success Criteria** (what must be TRUE):
1. `read_query_result` Tool 可被 AI 调用，参数包含 offset 和 limit
2. Tool 返回指定偏移量开始的指定条数数据
3. Tool 返回 JSON Lines 格式的原始数据，AI 可继续分析
4. Tool 在文件不存在或读取失败时返回明确的错误信息

**Plans**: TBD

---

## Progress Table

| Phase | Plans Complete | Status | Completed |
|-------|----------------|--------|-----------|
| 1. SSE 心跳 | 1/1 | Complete   | 2026-03-20 |
| 2. 临时文件存储 | 0/2 | Not started | - |
| 3. 随机读取 Tool | 0/1 | Not started | - |

---

## Coverage

**Requirements Coverage:** 4/4 (100%)

| Requirement | Phase | Description |
|-------------|-------|-------------|
| HEARTBEAT-01 | Phase 1 | SSE 连接每 10 秒自动发送空消息心跳 |
| TEMPFILE-01 | Phase 2 | SQL 查询结果超过配置阈值时存储为 JSON Lines 临时文件 |
| TEMPFILE-02 | Phase 2 | 临时文件在单轮会话结束时自动删除 |
| READERTOOL-01 | Phase 3 | 创建 read_query_result Tool，支持按偏移量读取 |

---

## Dependency Graph

```
[HEARTBEAT-01]
     │
     ▼
[TEMPFILE-01] ──► [TEMPFILE-02]
     │                    │
     └────────────────────┘
            │
            ▼
     [READERTOOL-01]
```

---

*Last updated: 2026-03-20*
