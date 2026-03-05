# ChatBI 知识库 RAG 和 ask_user tool 设计文档

## 1. 概述

本文档描述 ChatBI 模块的知识库 RAG（检索增强生成）功能和 ask_user tool（向用户提问工具）的设计方案。

### 1.1 目标

1. **知识库 RAG**：让 AI 在回答用户问题时，能够从知识库中检索相关内容进行增强
2. **ask_user tool**：让 AI 能够在需要时暂停对话，向用户询问缺失信息

### 1.2 方案选择

采用**分阶段实现**方案：
- 阶段 1：知识库 RAG（优先级：高）
- 阶段 2：ask_user tool（优先级：中）

---

## 2. 阶段 1：知识库 RAG

### 2.1 架构设计

```
┌─────────────────────────────────────────────────────────────────┐
│                         ChatBI 服务                              │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐    │
│  │  ChatAgent   │───▶│  RAG Service │───▶│  Milvus      │    │
│  │              │    │              │    │  向量数据库  │    │
│  └──────────────┘    └──────────────┘    └──────────────┘    │
│         │                    │                                   │
│         ▼                    ▼                                   │
│  ┌──────────────┐    ┌──────────────┐                          │
│  │ 知识库管理 API │    │ Embedding    │                          │
│  │              │    │ 服务          │                          │
│  └──────────────┘    └──────────────┘                          │
└─────────────────────────────────────────────────────────────────┘
```

### 2.2 数据模型

#### 2.2.1 知识库实体（KnowledgeBase）

| 字段 | 类型 | 说明 |
|------|------|------|
| id | Long | 主键 |
| name | String | 知识库名称 |
| description | String | 描述 |
| embeddingModelType | String | Embedding 模型类型 |
| createdAt | LocalDateTime | 创建时间 |
| updatedAt | LocalDateTime | 更新时间 |

#### 2.2.2 知识库内容实体（KnowledgeContent）

| 字段 | 类型 | 说明 |
|------|------|------|
| id | Long | 主键 |
| knowledgeBaseId | Long | 所属知识库 ID |
| title | String | 标题 |
| content | String | 内容（文档或代码） |
| contentType | String | 内容类型：DOCUMENT / CODE |
| vectorId | String | Milvus 中的向量 ID |
| createdAt | LocalDateTime | 创建时间 |
| updatedAt | LocalDateTime | 更新时间 |

### 2.3 核心模块设计

#### 2.3.1 Embedding 服务

```java
public interface EmbeddingService {
    /**
     * 生成文本的 Embedding 向量
     * @param text 文本内容
     * @param modelType 模型类型
     * @return 向量列表
     */
    List<Float> embed(String text, String modelType);

    /**
     * 批量生成 Embedding 向量
     * @param texts 文本列表
     * @param modelType 模型类型
     * @return 向量列表
     */
    List<List<Float>> embedBatch(List<String> texts, String modelType);
}
```

支持的模型类型：
- OPENAI (text-embedding-3-small)
- CLAUDE
- OLLAMA
- GEMINI

#### 2.3.2 向量检索服务

```java
public interface VectorSearchService {
    /**
     * 插入向量数据
     * @param collectionName 集合名称
     * @param vector 向量
     * @param metadata 元数据
     * @return 向量 ID
     */
    String insert(String collectionName, List<Float> vector, Map<String, Object> metadata);

    /**
     * 相似度搜索
     * @param collectionName 集合名称
     * @param queryVector 查询向量
     * @param topK 返回数量
     * @return 搜索结果
     */
    List<SearchResult> search(String collectionName, List<Float> queryVector, int topK);

    /**
     * 删除向量
     * @param collectionName 集合名称
     * @param vectorId 向量 ID
     */
    void delete(String collectionName, String vectorId);
}
```

#### 2.3.3 RAG 服务

```java
public interface RagService {
    /**
     * 检索相关知识
     * @param query 用户问题
     * @param knowledgeBaseId 知识库 ID
     * @param topK 返回数量
     * @return 相关知识列表
     */
    List<KnowledgeContent> retrieve(String query, Long knowledgeBaseId, int topK);

    /**
     * 添加知识内容
     * @param content 知识内容
     * @return 保存后的内容
     */
    KnowledgeContent addContent(KnowledgeContent content);

    /**
     * 删除知识内容
     * @param contentId 内容 ID
     */
    void deleteContent(Long contentId);
}
```

### 2.4 API 设计

#### 2.4.1 知识库管理

| 方法 | 路径 | 说明 |
|------|------|------|
| POST | /api/v1/ai/knowledge-base | 创建知识库 |
| GET | /api/v1/ai/knowledge-base | 获取知识库列表 |
| GET | /api/v1/ai/knowledge-base/{id} | 获取知识库详情 |
| PUT | /api/v1/ai/knowledge-base/{id} | 更新知识库 |
| DELETE | /api/v1/ai/knowledge-base/{id} | 删除知识库 |

#### 2.4.2 知识内容管理

| 方法 | 路径 | 说明 |
|------|------|------|
| POST | /api/v1/ai/knowledge-base/{id}/content | 添加知识内容 |
| GET | /api/v1/ai/knowledge-base/{id}/content | 获取知识内容列表 |
| DELETE | /api/v1/ai/knowledge-content/{id} | 删除知识内容 |
| POST | /api/v1/ai/knowledge-base/{id}/content/batch-import | 批量导入知识内容 |

#### 2.4.3 知识检索

| 方法 | 路径 | 说明 |
|------|------|------|
| GET | /api/v1/ai/knowledge-base/{id}/search | 检索知识 |

### 2.5 前端设计

#### 2.5.1 知识库管理页面

- 知识库列表
- 创建/编辑知识库
- 删除知识库

#### 2.5.2 知识内容管理页面

- 内容列表（支持分页、搜索）
- 添加/编辑内容
- 批量导入
- 删除内容

---

## 3. 阶段 2：ask_user tool

### 3.1 架构设计

```
┌─────────────────────────────────────────────────────────────────┐
│                         ChatBI 对话流程                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   ┌──────────┐    ┌──────────┐    ┌──────────┐                │
│   │ 用户输入  │───▶│ Agent    │───▶│ 判定     │                │
│   └──────────┘    │ 处理      │    │ 需要询问  │                │
│                   └──────────┘    └────┬─────┘                │
│                                           │                     │
│                   ┌──────────────────────┴──────────────┐     │
│                   │                                      │     │
│                   ▼                                      ▼     │
│          ┌────────────────┐              ┌────────────────┐   │
│          │  继续执行       │              │ ask_user tool │   │
│          │  生成回答      │              │  暂停等待用户  │   │
│          └────────────────┘              └────────────────┘   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### 3.2 Tool 设计

#### 3.2.1 Tool 接口

```java
public interface Tool {
    String getName();
    String getDescription();
    Map<String, Object> execute(Map<String, Object> parameters);
}
```

#### 3.2.2 ask_user tool 实现

```java
@Component
public class AskUserTool implements Tool {

    @Override
    public String getName() {
        return "ask_user";
    }

    @Override
    public String getDescription() {
        return "当 AI 无法直接回答问题或缺少必要信息时，向用户提问获取更多信息";
    }

    @Override
    public Map<String, Object> execute(Map<String, Object> parameters) {
        // 参数说明：
        // - question: 需要询问的问题
        // - questionType: 问题类型（TEXT / SELECT / DATE / NUMBER）
        // - options: 选项列表（当 questionType 为 SELECT 时必填）
        // - required: 是否必填
        // - context: 上下文信息，帮助用户理解问题
    }
}
```

### 3.3 交互类型

| 类型 | 说明 | 参数 |
|------|------|------|
| TEXT | 文本输入 | - |
| SELECT | 单选 | options（选项列表） |
| MULTI_SELECT | 多选 | options（选项列表） |
| DATE | 日期选择 | - |
| NUMBER | 数字输入 | min, max（可选） |

### 3.4 对话状态管理

```java
public enum ChatStatus {
    RUNNING,    // 运行中
    WAITING,    // 等待用户输入
    COMPLETED   // 完成
}
```

### 3.5 前端设计

#### 3.5.1 用户交互组件

- 文本输入框
- 下拉选择器
- 日期选择器
- 数字输入框

---

## 4. 配置文件

### 4.1 application-ai.properties

```properties
# Milvus 配置
milvus.host=localhost
milvus.port=19530
milvus.collection.name=chatbi_knowledge
```

---

## 5. 实现计划

### 阶段 1：知识库 RAG（约 2 周）

1. [ ] 创建知识库实体和数据库表
2. [ ] 实现 Embedding 服务接口
3. [ ] 集成 Milvus 向量数据库
4. [ ] 实现 RAG 检索服务
5. [ ] 开发知识库管理 API
6. [ ] 开发知识内容管理 API
7. [ ] 前端知识库管理页面
8. [ ] 前端知识内容管理页面

### 阶段 2：ask_user tool（约 1 周）

1. [ ] 设计 Tool 接口和基类
2. [ ] 实现 ask_user_tool
3. [ ] 修改 ChatAgent 对话流程
4. [ ] 实现对话状态管理
5. [ ] 前端用户交互组件

---

## 6. 风险与挑战

1. **Milvus 连接配置**：需要确保 Milvus 服务可用
2. **Embedding 成本**：不同模型的调用成本不同
3. **向量检索效果**：需要调优 topK 和相似度阈值
4. **用户体验**：ask_user 的触发时机需要谨慎设计
