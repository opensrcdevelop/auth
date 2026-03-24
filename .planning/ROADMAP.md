# Roadmap: ChatBI 多向量数据库与思考过程控制

## Proposed Roadmap

**2 phases** | **7 requirements mapped** | All v1 requirements covered ✓

| # | Phase | Goal | Requirements | Success Criteria |
|---|-------|------|--------------|-----------------|
| 1 | 思考过程控制 | 前端可开关思考过程展示 | THINK-01, THINK-02, THINK-03 | 3 |
| 2 | Chroma 向量数据库支持 | 支持 Chroma 向量数据库 | VDB-01, VDB-02, VDB-03, VDB-04 | 4 |

### Phase Details

**Phase 1: 思考过程控制**
Goal: 用户可在对话界面控制思考过程的展示，并记住偏好
Requirements: THINK-01, THINK-02, THINK-03
Success criteria:
1. 对话界面显示思考过程开关（默认开启）
2. 关闭后前端不展示 THINKING 消息
3. 用户偏好存储到后端并在下一次对话时恢复

**Phase 2: Chroma 向量数据库支持**
Goal: 通过配置切换支持 Chroma 向量数据库
Requirements: VDB-01, VDB-02, VDB-03, VDB-04
Success criteria:
1. 配置项支持选择向量数据库类型（milvus/chroma）
2. Chroma 实现与现有 Milvus 实现接口一致
3. Collection 命名和 Schema 与 Milvus 保持一致
4. 切换向量数据库类型后现有功能不受影响

---

## Phase State

| Phase | Status | Plans | Progress |
|-------|--------|-------|----------|
| 1 | ✓ Complete | 3/3 | 100% |
| 2 | ◐ Planned | 1/1 | 100% |

---

## Plans

### Phase 1: 思考过程控制

Plans:
- [x] 01-00-PLAN.md — Wave 0: ThinkAnswerAgentTest 测试骨架
- [x] 01-01-PLAN.md — 前端开关 UI + localStorage 持久化
- [x] 01-02-PLAN.md — 后端 showThinking 传播链路 + 思考消息门控

### Phase 2: Chroma 向量数据库支持

Plans:
- [x] 02-01-PLAN.md — Chroma 向量数据库配置和接口实现

---

*Roadmap created: 2026-03-24*
*Last updated: 2026-03-24 after 02-01 plan execution*
