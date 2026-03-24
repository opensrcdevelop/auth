# Requirements: ChatBI 多向量数据库与思考过程控制

**Defined:** 2026-03-24
**Core Value:** 在保持现有 ChatBI 功能稳定的前提下，通过配置化支持多种向量数据库，并通过用户可控的思考过程展示提升交互体验。

## v1 Requirements

### 思考过程控制

- [ ] **THINK-01**: 用户可在对话界面开关思考过程展示（默认开启）
- [ ] **THINK-02**: 系统记住用户对思考过程展示的偏好设置
- [ ] **THINK-03**: 关闭后前端不展示 THINKING 类型消息

### 向量数据库

- [x] **VDB-01**: 支持配置选择向量数据库类型（Milvus 或 Chroma）
- [x] **VDB-02**: 实现 Chroma 向量数据库的 SampleSqlVectorStoreService
- [x] **VDB-03**: Chroma Collection 命名与 Milvus 一致（sample_sql_{tenantCode}）
- [x] **VDB-04**: Chroma Schema 与 Milvus 一致（id, answer_id, question, sql, data_source_id, created_at, question_vector）

## v2 Requirements

- **VDB-05**: 向量数据库连接池管理优化
- **VDB-06**: 支持向量数据库健康检查

## Out of Scope

| Feature | Reason |
|---------|--------|
| Pinecone、Weaviate 等其他向量数据库 | 当前需求仅限 Chroma |
| 向量数据库自动切换/负载均衡 | 第一期仅支持静态配置切换 |
| 思考过程内容自定义模板 | 第一期仅开关控制 |

## Traceability

| Requirement | Phase | Status |
|-------------|-------|--------|
| THINK-01 | — | Pending |
| THINK-02 | — | Pending |
| THINK-03 | — | Pending |
| VDB-01 | 02-chroma-vector-database | Complete |
| VDB-02 | 02-chroma-vector-database | Complete |
| VDB-03 | 02-chroma-vector-database | Complete |
| VDB-04 | 02-chroma-vector-database | Complete |

**Coverage:**
- v1 requirements: 7 total
- Mapped to phases: 4
- Complete: 4 (VDB-01, VDB-02, VDB-03, VDB-04)
- Pending: 3 (THINK-01, THINK-02, THINK-03)

---
*Requirements defined: 2026-03-24*
*Last updated: 2026-03-24 after 02-01 plan execution*
