# Roadmap: ChatBI 连接增强

## Milestones

- ✅ **v1.0 MVP** — Phases 1-3 (shipped 2026-03-20)
- 🚧 **v1.1 Quality Fix** — Phase 4 (in progress)

## Phases

<details>
<summary>✅ v1.0 MVP (Phases 1-3) — SHIPPED 2026-03-20</summary>

- [x] Phase 1: SSE 心跳 (1/1 plans) — completed 2026-03-20
- [x] Phase 2: 临时文件存储 (2/2 plans) — completed 2026-03-20
- [x] Phase 3: 随机读取 Tool (1/1 plans) — completed 2026-03-20

</details>

### 🚧 v1.1 Quality Fix (In Progress)

| Phase | Status | Plans | Description |
|-------|--------|-------|-------------|
| 4. 质量修复 | Planned | 1 | 修复 v1.0 实现质量问题 |

**Phase 4 Goals:**
- 修复 ExecuteSqlTool 不应清除 ChatContext.queryData
- 重命名 TempFileManager → QueryResultTempFileManager
- 属性命名规范化为 chatbi.query-result.*
- 支持多次 SQL 执行的 temp 文件清理
- 添加 application-ai.properties 配置

## Progress

| Phase | Milestone | Plans Complete | Status | Completed |
|-------|-----------|----------------|--------|-----------|
| 1. SSE 心跳 | v1.0 | 1/1 | Complete | 2026-03-20 |
| 2. 临时文件存储 | v1.0 | 2/2 | Complete | 2026-03-20 |
| 3. 随机读取 Tool | v1.0 | 1/1 | Complete | 2026-03-20 |
| 4. 质量修复 | v1.1 | 0/1 | Planned | - |

---

*Last updated: 2026-03-20 after v1.0 milestone*
