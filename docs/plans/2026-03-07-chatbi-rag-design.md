# ChatBI 示例 SQL 向量检索（RAG）设计方案

## 1. 背景与目标

### 1.1 背景

当前 ChatBI 模块已实现示例 SQL 功能，用于在生成 SQL 时提供参考示例。现有实现存在以下问题：

1. **检索效率低**：从历史 LIKE 回答中检索相关示例时，需要先将所有历史问题发送给 Agent 判断相关性，500 条历史记录需要多次调用 LLM
2. **扩展性差**：无法快速添加自定义示例 SQL
3. **缺乏管理界面**：无法可视化管理示例 SQL

### 1.2 目标

1. 使用向量数据库（Milvus）实现 RAG 检索，提升检索效率
2. 支持手动添加/删除示例 SQL
3. 支持从用户反馈（LIKE）中自动同步示例 SQL
4. 提供示例 SQL 管理前端页面

## 2. 架构设计

### 2.1 整体架构

```
┌─────────────────────────────────────────────────────────────┐
│                        前端 (Vue 3)                          │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐  │
│  │  ChatBI 对话 │  │ 示例SQL管理  │  │  系统配置（嵌入模型） │  │
│  └─────────────┘  └─────────────┘  └─────────────────────┘  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                     Auth Server (Spring Boot)                │
│  ┌─────────────────────────────────────────────────────────┐│
│  │                    ChatBIController                     ││
│  │  - /chatbi/sampleSql/* (示例 SQL 管理)                 ││
│  │  - /chatbi/chat/stream (对话，含 RAG 检索)             ││
│  │  - /chatbi/answer/vote (投票，自动同步向量库)           ││
│  └─────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      ai-chatbi 模块                          │
│  ┌────────────────┐  ┌────────────────┐  ┌──────────────┐  │
│  │ SampleSqlService│  │ EmbeddingService│  │ MilvusService│ │
│  │  - CRUD         │  │  - 获取嵌入向量  │  │  - 向量存储   │  │
│  │  - 同步管理     │  │  - 模型调用     │  │  - 向量检索   │  │
│  └────────────────┘  └────────────────┘  └──────────────┘  │
│                                                              │
│  ┌────────────────────────────────────────────────────────┐ │
│  │              SqlAgent (RAG 检索示例 SQL)               │ │
│  │  - getSampleSqls() → 向量检索                          │ │
│  └────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
          │                                    │
          ▼                                    ▼
┌─────────────────────┐          ┌─────────────────────────┐
│  System Setting     │          │      Milvus             │
│  (t_sys_setting)   │          │  (向量数据库)            │
│  - chatbi.embedding│          │  - 租户级 Collection    │
│    .provider.id     │          │    sample_sql_{tenant}  │
│  - chatbi.embedding│          │  - 字段: id, answer_id, │
│    .similarity     │          │    question, sql,        │
│    .threshold      │          │    data_source_id,       │
│                    │          │    question_vector       │
└─────────────────────┘          └─────────────────────────┘
```

### 2.2 组件说明

| 组件 | 职责 |
|-----|------|
| `SampleSqlService` | 示例 SQL 的 CRUD 管理，自动同步逻辑 |
| `EmbeddingService` | 调用嵌入模型获取文本向量 |
| `MilvusService` | 向量数据库的插入、删除、检索操作 |
| `SqlAgent` | 在生成 SQL 时检索相关示例 |

## 3. 数据模型

### 3.1 系统配置（SystemSetting）

| Key | 说明 | 示例值 |
|-----|------|-------|
| `chatbi.embedding.provider.id` | 嵌入模型提供商 ID | `provider-xxx` |
| `chatbi.embedding.similarity.threshold` | 相似度阈值 | `0.7` |

### 3.2 Milvus Collection

**Collection 命名**: `sample_sql_{tenant_code}`

**字段结构**:

| 字段 | 类型 | 说明 |
|-----|------|------|
| `id` | VarChar(64) | 主键（UUID） |
| `answer_id` | VarChar(64) | 回答 ID（用于删除操作） |
| `question` | VarChar(2000) | 问题文本 |
| `sql` | VarChar(4000) | SQL 语句 |
| `data_source_id` | VarChar(64) | 数据源 ID（必填，用于检索过滤） |
| `question_vector` | FloatVector(1536) | 问题向量化（维度由嵌入模型决定） |
| `created_at` | Datetime | 创建时间 |

## 4. API 设计

### 4.1 示例 SQL 管理

| 方法 | 路径 | 说明 |
|-----|------|------|
| GET | `/chatbi/sampleSql/list` | 获取示例 SQL 列表（支持按数据源筛选） |
| POST | `/chatbi/sampleSql` | 添加示例 SQL |
| DELETE | `/chatbi/sampleSql/{id}` | 删除示例 SQL |
| POST | `/chatbi/sampleSql/syncFromLikes` | 从 LIKE 反馈同步到向量库 |
| POST | `/chatbi/sampleSql/rebuild` | 重建向量索引（全量同步） |

### 4.2 配置管理

| 方法 | 路径 | 说明 |
|-----|------|------|
| GET | `/chatbi/embedding/config` | 获取嵌入配置 |
| PUT | `/chatbi/embedding/config` | 更新嵌入配置 |

### 4.3 对话接口（现有接口，修改内部实现）

| 方法 | 路径 | 说明 |
|-----|------|------|
| POST | `/chatbi/chat/stream` | 对话（内部调用 RAG 检索） |

### 4.4 投票接口（现有接口，添加同步逻辑）

| 方法 | 路径 | 说明 |
|-----|------|------|
| POST | `/chatbi/answer/vote` | 投票（LIKE 时同步到向量库，UNLIKE 时从向量库删除） |

## 5. 核心流程

### 5.1 RAG 检索示例 SQL

```
用户提问
    │
    ▼
SqlAgent.getSampleSqls(dataSourceId, question)
    │
    ▼
获取当前租户 Code → collection: sample_sql_{tenant_code}
    │
    ▼
EmbeddingService.embedQuestion(question)
    │
    ▼
MilvusService.search(
    collection=sample_sql_{tenant_code},
    vector=questionVector,
    filter="data_source_id == '{dataSourceId}'",
    threshold=xxx
)
    │
    ▼
返回相似的问题-SQL 对列表
```

### 5.2 投票同步逻辑

```
投票 LIKE:
├── 获取当前回答的 answerId、question、sql、dataSourceId
├── 获取当前租户 Code
├── EmbeddingService.embedQuestion(question)
├── MilvusService.insert(
│       collection=sample_sql_{tenant_code},
│       data={
│           id: UUID,
│           answer_id: answerId,
│           question: question,
│           sql: sql,
│           data_source_id: dataSourceId,
│           question_vector: vector,
│           created_at: now()
│       }
│   )

投票 UNLIKE:
└── MilvusService.deleteByAnswerId(
        collection=sample_sql_{tenant_code},
        answerId: answerId
    )
```

### 5.3 重建索引（全量同步）

```
重建索引:
├── 获取当前租户下所有 LIKE 反馈的回答
├── 遍历每个回答:
│   ├── 获取 answerId、question、sql、dataSourceId
│   ├── 生成向量
│   └── 插入 Milvus
└── 完成重建
```

## 6. 前端页面设计

### 6.1 Tab 结构调整

在现有的三个 Tab 基础上添加示例 SQL 管理 Tab：

1. 对话问答（Chat）
2. 数据源管理（DataSource）
3. LLM 管理（LLM）
4. **示例 SQL 管理（Sample SQL）** ← 新增

### 6.2 页面布局

```
┌─────────────────────────────────────────────────────────────┐
│  示例 SQL 管理                                               │
├─────────────────────────────────────────────────────────────┤
│  数据源: [全部 ▼]  嵌入模型: [选择提供商 ▼]  阈值: [0.7]     │
│                                          [保存配置]        │
├─────────────────────────────────────────────────────────────┤
│  [+ 添加示例]  [从 Likes 同步]  [重建索引]                   │
├─────────────────────────────────────────────────────────────┤
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ ID        │ 数据源   │ 问题              │ SQL         ││
│ ├───────────┼──────────┼───────────────────┼─────────────┤ │
│ │ uuid-001  │ ds-mysql │ 查询本月销售额    │ SELECT ...  ││
│ │ uuid-002  │ ds-pg    │ 用户增长趋势如何  │ SELECT ...  ││
│ └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

### 6.3 添加示例弹窗

```
┌────────────────────────────────────────┐
│  添加示例 SQL                          │
├────────────────────────────────────────┤
│  数据源:   [选择数据源 ▼]  *           │
│  问题:     [输入问题文本     ]  *      │
│  SQL:     [输入 SQL 语句     ]  *      │
│                                        │
│         [取消]        [确定]            │
└────────────────────────────────────────┘
```

## 7. 配置说明

### 7.1 application-ai.properties

```properties
# Milvus 配置
milvus.host=localhost
milvus.port=19530
```

### 7.2 嵌入模型配置

通过前端页面配置到 System Setting 表：

| Key | 说明 |
|-----|------|
| `chatbi.embedding.provider.id` | 模型提供商 ID（从 t_model_provider 选择） |
| `chatbi.embedding.similarity.threshold` | 相似度阈值（0.0-1.0） |

## 8. 错误处理

| 场景 | 处理方式 |
|-----|---------|
| Milvus 连接失败 | 记录日志，返回空示例列表，不影响主流程 |
| 嵌入模型调用失败 | 记录日志，返回空示例列表 |
| 未配置嵌入模型 | 跳过 RAG 检索，使用原有逻辑 |
| 投票同步失败 | 记录重试日志，不影响投票结果 |

## 9. 迁移计划

1. **第一阶段**：添加 Milvus 服务、Embedding 服务、SampleSql 服务
2. **第二阶段**：修改 SqlAgent 使用向量检索
3. **第三阶段**：添加投票同步逻辑
4. **第四阶段**：添加前端页面
5. **第五阶段**：测试和优化

## 10. 待确定事项

- 嵌入模型维度（取决于选择的模型）
- Milvus 索引类型（HNSW、IVF_FLAT 等）
