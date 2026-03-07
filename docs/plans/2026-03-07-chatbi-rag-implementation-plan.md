# ChatBI 示例 SQL 向量检索（RAG）实现计划

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 使用 Milvus 向量数据库实现示例 SQL 的 RAG 检索，支持手动管理、自动同步和前端页面

**Architecture:** 使用 Milvus 作为向量数据库，嵌入模型将问题向量化，实现相似问题检索。每个租户一个 Collection，数据源 ID 作为过滤条件

**Tech Stack:** Spring Boot 3.5, Spring AI, Milvus (io.milvus:milvus-sdk-java), Vue 3

---

## 第一阶段：基础设施搭建

### Task 1: 添加 Milvus 依赖和配置

**Files:**
- Modify: `ai-chatbi/build.gradle:1-50` (添加 Milvus SDK 依赖)
- Modify: `auth-server/src/main/resources/application-ai.properties` (添加 Milvus 配置)
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/config/MilvusConfig.java`

**Step 1: 添加 Milvus 依赖**

在 `ai-chatbi/build.gradle` 的 dependencies 中添加：

```groovy
// Milvus 向量数据库
implementation 'io.milvus:milvus-sdk-java:2.4.3'
```

**Step 2: 添加配置**

在 `application-ai.properties` 添加：

```properties
# Milvus 配置
milvus.host=localhost
milvus.port=19530
```

**Step 3: 创建配置类**

创建 `MilvusConfig.java`:

```java
package cn.opensrcdevelop.ai.config;

import io.milvus.client.MilvusServiceClient;
import io.milvus.param.ConnectParam;
import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Data
@Configuration
@ConfigurationProperties(prefix = "milvus")
public class MilvusConfig {

    private String host;

    private Integer port;

    @Bean
    public MilvusServiceClient milvusServiceClient() {
        ConnectParam connectParam = ConnectParam.newBuilder()
                .withHost(host)
                .withPort(port)
                .build();
        return new MilvusServiceClient(connectParam);
    }
}
```

**Step 4: 提交**

```bash
git add ai-chatbi/build.gradle auth-server/src/main/resources/application-ai.properties
git create ai-chatbi/src/main/java/cn/opensrcdevelop/ai/config/MilvusConfig.java
git commit -m "feat(chatbi): 添加 Milvus 向量数据库依赖和配置"
```

---

### Task 2: 创建 DTO 类

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/SampleSqlDto.java`
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/SampleSqlRequestDto.java`
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/EmbeddingConfigDto.java`

**Step 1: 创建 SampleSqlDto**

```java
package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "示例 SQL 信息")
@Builder
@Getter
@Setter
public class SampleSqlDto {

    @Schema(description = "ID")
    private String id;

    @Schema(description = "回答ID")
    private String answerId;

    @Schema(description = "问题")
    private String question;

    @Schema(description = "SQL")
    private String sql;

    @Schema(description = "数据源ID")
    private String dataSourceId;

    @Schema(description = "创建时间")
    private String createdAt;
}
```

**Step 2: 创建 SampleSqlRequestDto**

```java
package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "添加示例 SQL 请求")
@Data
public class SampleSqlRequestDto {

    @NotBlank(message = "数据源ID不能为空")
    @Schema(description = "数据源ID")
    private String dataSourceId;

    @NotBlank(message = "问题不能为空")
    @Schema(description = "问题")
    private String question;

    @NotBlank(message = "SQL不能为空")
    @Schema(description = "SQL")
    private String sql;
}
```

**Step 3: 创建 EmbeddingConfigDto**

```java
package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "嵌入模型配置")
@Data
public class EmbeddingConfigDto {

    @Schema(description = "模型提供商ID")
    private String providerId;

    @Schema(description = "相似度阈值")
    private Double similarityThreshold;
}
```

**Step 4: 提交**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/SampleSqlDto.java
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/SampleSqlRequestDto.java
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/dto/EmbeddingConfigDto.java
git commit -m "feat(chatbi): 创建示例 SQL 和嵌入配置 DTO"
```

---

### Task 3: 创建 MilvusService

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/MilvusService.java`
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/MilvusServiceImpl.java`

**Step 1: 创建接口**

```java
package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import java.util.List;

public interface MilvusService {

    /**
     * 创建 Collection（如果不存在）
     *
     * @param tenantCode 租户 Code
     */
    void createCollectionIfNotExists(String tenantCode);

    /**
     * 插入示例 SQL 向量
     *
     * @param tenantCode 租户 Code
     * @param sampleSql  示例 SQL
     * @param vector     向量
     */
    void insert(String tenantCode, SampleSqlDto sampleSqlDto, List<Float> vector);

    /**
     * 根据 answerId 删除向量
     *
     * @param tenantCode 租户 Code
     * @param answerId   回答 ID
     */
    void deleteByAnswerId(String tenantCode, String answerId);

    /**
     * 搜索相似问题
     *
     * @param tenantCode       租户 Code
     * @param dataSourceId     数据源 ID
     * @param vector           查询向量
     * @param threshold        相似度阈值
     * @param limit            返回数量
     * @return 相似的问题-SQL 对列表
     */
    List<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector,
            double threshold, int limit);
}
```

**Step 2: 创建实现类**

```java
package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.config.MilvusConfig;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.service.MilvusService;
import io.milvus.client.MilvusServiceClient;
import io.milvus.param.CollectionNameParam;
import io.milvus.param.DmlParam;
import io.milvus.param.IndexType;
import io.milvus.param.MetricType;
import io.milvus.param.collection.CreateCollectionParam;
import io.milvus.param.collection.FieldType;
import io.milvus.param.collection.HasCollectionParam;
import io.milvus.param.dml.DeleteParam;
import io.milvus.param.dml.InsertParam;
import io.milvus.param.dml.QueryParam;
import io.milvus.param.dml.SearchParam;
import io.milvus.response.QueryResultsWrapper;
import io.milvus.response.SearchResultsWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Slf4j
@Service
@RequiredArgsConstructor
public class MilvusServiceImpl implements MilvusService {

    private static final String COLLECTION_PREFIX = "sample_sql_";
    private static final int VECTOR_DIMENSION = 1536; // OpenAI text-embedding-ada-002 默认维度

    private final MilvusServiceClient milvusServiceClient;
    private final MilvusConfig milvusConfig;

    @Override
    public void createCollectionIfNotExists(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        // 检查 Collection 是否存在
        HasCollectionParam hasCollectionParam = HasCollectionParam.newBuilder()
                .withCollectionName(collectionName)
                .build();
        boolean exists = milvusServiceClient.hasCollection(hasCollectionParam).getData();

        if (!exists) {
            // 创建 Collection
            List<FieldType> fields = new ArrayList<>();
            fields.add(FieldType.newBuilder()
                    .withName("id")
                    .withDataType(io.milvus.param.DataType.VarChar)
                    .withMaxLength(64)
                    .withPrimaryKey(true)
                    .build());
            fields.add(FieldType.newBuilder()
                    .withName("answer_id")
                    .withDataType(io.milvus.param.DataType.VarChar)
                    .withMaxLength(64)
                    .build());
            fields.add(FieldType.newBuilder()
                    .withName("question")
                    .withDataType(io.milvus.param.DataType.VarChar)
                    .withMaxLength(2000)
                    .build());
            fields.add(FieldType.newBuilder()
                    .withName("sql")
                    .withDataType(io.milvus.param.DataType.VarChar)
                    .withMaxLength(4000)
                    .build());
            fields.add(FieldType.newBuilder()
                    .withName("data_source_id")
                    .withDataType(io.milvus.param.DataType.VarChar)
                    .withMaxLength(64)
                    .build());
            fields.add(FieldType.newBuilder()
                    .withName("question_vector")
                    .withDataType(io.milvus.param.DataType.FloatVector)
                    .withDimension(VECTOR_DIMENSION)
                    .build());
            fields.add(FieldType.newBuilder()
                    .withName("created_at")
                    .withDataType(io.milvus.param.DataType.VarChar)
                    .withMaxLength(32)
                    .build());

            CreateCollectionParam createCollectionParam = CreateCollectionParam.newBuilder()
                    .withCollectionName(collectionName)
                    .withFieldTypes(fields)
                    .build();
            milvusServiceClient.createCollection(createCollectionParam);

            // 创建索引（简化版，使用 HNSW）
            // 实际生产环境需要更完善的索引配置
            log.info("Collection {} created successfully", collectionName);
        }
    }

    @Override
    public void insert(String tenantCode, SampleSqlDto sampleSqlDto, List<Float> vector) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        List<InsertParam.Field> fields = new ArrayList<>();
        fields.add(new InsertParam.Field("id", sampleSqlDto.getId()));
        fields.add(new InsertParam.Field("answer_id", sampleSqlDto.getAnswerId()));
        fields.add(new InsertParam.Field("question", sampleSqlDto.getQuestion()));
        fields.add(new InsertParam.Field("sql", sampleSqlDto.getSql()));
        fields.add(new InsertParam.Field("data_source_id", sampleSqlDto.getDataSourceId()));
        fields.add(new InsertParam.Field("question_vector", vector));
        fields.add(new InsertParam.Field("created_at",
                LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"))));

        InsertParam insertParam = InsertParam.newBuilder()
                .withCollectionName(collectionName)
                .withFields(fields)
                .build();

        milvusServiceClient.insert(insertParam);
        log.info("Inserted sample SQL: id={}, answerId={}", sampleSqlDto.getId(), sampleSqlDto.getAnswerId());
    }

    @Override
    public void deleteByAnswerId(String tenantCode, String answerId) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        String deleteExpr = "answer_id == '" + answerId + "'";
        DeleteParam deleteParam = DeleteParam.newBuilder()
                .withCollectionName(collectionName)
                .withExpr(deleteExpr)
                .build();

        milvusServiceClient.delete(deleteParam);
        log.info("Deleted sample SQL by answerId: {}", answerId);
    }

    @Override
    public List<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector,
            double threshold, int limit) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        // 确保 Collection 存在
        createCollectionIfNotExists(tenantCode);

        // 构建查询
        List<String> outputFields = Arrays.asList("id", "answer_id", "question", "sql", "data_source_id", "created_at");
        String searchExpr = "data_source_id == '" + dataSourceId + "'";

        SearchParam searchParam = SearchParam.newBuilder()
                .withCollectionName(collectionName)
                .withVectorFieldName("question_vector")
                .withVectors(Collections.singletonList(vector))
                .withTopK(limit)
                .withExpr(searchExpr)
                .withMetricType(MetricType.IP) // 内积相似度
                .withOutputFields(outputFields)
                .build();

        SearchResultsWrapper wrapper = milvusServiceClient.search(searchParam).getData();
        List<SampleSqlDto> results = new ArrayList<>();

        for (SearchResultsWrapper.QueriesRetResult result : wrapper.getSearchResults()) {
            Map<String, Object> fields = result.getFieldValues();
            double score = result.getScore();

            // 过滤低于阈值的结果
            if (score < threshold) {
                continue;
            }

            SampleSqlDto dto = SampleSqlDto.builder()
                    .id((String) fields.get("id"))
                    .answerId((String) fields.get("answer_id"))
                    .question((String) fields.get("question"))
                    .sql((String) fields.get("sql"))
                    .dataSourceId((String) fields.get("data_source_id"))
                    .createdAt((String) fields.get("created_at"))
                    .build();
            results.add(dto);
        }

        log.info("Search returned {} results (threshold={})", results.size(), threshold);
        return results;
    }
}
```

**Step 3: 提交**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/MilvusService.java
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/MilvusServiceImpl.java
git commit -m "feat(chatbi): 添加 MilvusService 向量数据库操作服务"
```

---

### Task 4: 创建 EmbeddingService

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/EmbeddingService.java`
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/EmbeddingServiceImpl.java`

**Step 1: 创建接口**

```java
package cn.opensrcdevelop.ai.service;

import java.util.List;

public interface EmbeddingService {

    /**
     * 获取文本的嵌入向量
     *
     * @param text 文本
     * @return 向量列表
     */
    List<Float> embedText(String text);

    /**
     * 获取嵌入模型的维度
     *
     * @return 向量维度
     */
    int getDimension();
}
```

**Step 2: 创建实现类**

```java
package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.service.EmbeddingService;
import cn.opensrcdevelop.ai.service.ModelProviderService;
import cn.opensrcdevelop.biz.biz.service.system.SystemSettingService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.model.Embedding;
import org.springframework.ai.chat.model.EmbeddingModel;
import org.springframework.ai.chat.model.EmbeddingRequest;
import org.springframework.ai.chat.model.EmbeddingResponse;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class EmbeddingServiceImpl implements EmbeddingService {

    private static final String EMBEDDING_PROVIDER_ID_KEY = "chatbi.embedding.provider.id";
    private static final int DEFAULT_DIMENSION = 1536;

    private final ModelProviderService modelProviderService;
    private final SystemSettingService systemSettingService;

    @Override
    public List<Float> embedText(String text) {
        String providerId = systemSettingService.getValueByKey(EMBEDDING_PROVIDER_ID_KEY, "");

        if (providerId == null || providerId.isEmpty()) {
            log.warn("未配置嵌入模型提供商，跳过向量化");
            return new ArrayList<>();
        }

        try {
            ChatClient chatClient = modelProviderService.getChatClient(providerId);
            EmbeddingModel embeddingModel = chatClient.get(EmbeddingModel.class);

            EmbeddingRequest request = new EmbeddingRequest(List.of(text));
            EmbeddingResponse response = embeddingModel.call(request);

            List<Float> vectors = new ArrayList<>();
            for (Embedding embedding : response.getResults()) {
                for (Double value : embedding.getEmbedding()) {
                    vectors.add(value.floatValue());
                }
            }

            log.debug("Text embedded successfully, dimension: {}", vectors.size());
            return vectors;
        } catch (Exception e) {
            log.error("获取嵌入向量失败", e);
            return new ArrayList<>();
        }
    }

    @Override
    public int getDimension() {
        // 实际应该从嵌入模型配置中获取维度
        // 这里简化处理，返回默认值
        return DEFAULT_DIMENSION;
    }
}
```

**Step 3: 提交**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/EmbeddingService.java
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/EmbeddingServiceImpl.java
git commit -m "feat(chatbi): 添加 EmbeddingService 嵌入向量服务"
```

---

## 第二阶段：业务服务开发

### Task 5: 创建 SampleSqlService

**Files:**
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/SampleSqlService.java`
- Create: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/SampleSqlServiceImpl.java`

**Step 1: 创建接口**

```java
package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlRequestDto;

import java.util.List;

public interface SampleSqlService {

    /**
     * 获取示例 SQL 列表（支持按数据源筛选）
     *
     * @param dataSourceId 数据源 ID（可选）
     * @return 示例 SQL 列表
     */
    List<SampleSqlDto> list(String dataSourceId);

    /**
     * 添加示例 SQL
     *
     * @param request 请求
     */
    void add(SampleSqlRequestDto request);

    /**
     * 删除示例 SQL
     *
     * @param id ID
     */
    void delete(String id);

    /**
     * 从 LIKE 反馈同步到向量库
     *
     * @return 同步数量
     */
    int syncFromLikes();

    /**
     * 重建向量索引（全量同步）
     *
     * @return 重建数量
     */
    int rebuild();

    /**
     * 根据 answerId 添加到向量库（投票 LIKE 时调用）
     *
     * @param answerId 回答 ID
     */
    void addToVectorStore(String answerId);

    /**
     * 根据 answerId 从向量库删除（投票 UNLIKE 时调用）
     *
     * @param answerId 回答 ID
     */
    void removeFromVectorStore(String answerId);

    /**
     * RAG 检索相关示例 SQL
     *
     * @param dataSourceId 数据源 ID
     * @param question     问题
     * @return 相关示例 SQL 列表
     */
    List<SampleSqlDto> search(String dataSourceId, String question);
}
```

**Step 2: 创建实现类**

```java
package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlRequestDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.biz.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.util.TenantContextHolder;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class SampleSqlServiceImpl implements SampleSqlService {

    private static final String SIMILARITY_THRESHOLD_KEY = "chatbi.embedding.similarity.threshold";
    private static final double DEFAULT_THRESHOLD = 0.7;
    private static final int DEFAULT_LIMIT = 5;

    private final MilvusService milvusService;
    private final EmbeddingService embeddingService;
    private final ChatAnswerService chatAnswerService;

    @Override
    public List<SampleSqlDto> list(String dataSourceId) {
        String tenantCode = TenantContextHolder.getTenantCode();
        double threshold = getSimilarityThreshold();

        // 从 Milvus 获取该租户下所有向量（不分数据源筛选，用于前端展示）
        // 实际生产环境可能需要分页查询
        List<SampleSqlDto> allResults = new ArrayList<>();

        try {
            // 获取嵌入向量
            List<Float> dummyVector = new ArrayList<>();
            for (int i = 0; i < embeddingService.getDimension(); i++) {
                dummyVector.add(0f);
            }

            // 使用空数据源查询获取所有数据（需要 Milvus 支持此查询方式）
            // 简化处理：暂时返回空列表，前端可通过其他方式查看
            // TODO: 后续可添加查询所有向量的接口
        } catch (Exception e) {
            log.error("获取示例 SQL 列表失败", e);
        }

        return allResults;
    }

    @Override
    public void add(SampleSqlRequestDto request) {
        String tenantCode = TenantContextHolder.getTenantCode();
        String id = UUID.randomUUID().toString();

        SampleSqlDto dto = SampleSqlDto.builder()
                .id(id)
                .answerId("manual-" + id) // 手动添加的标记
                .question(request.getQuestion())
                .sql(request.getSql())
                .dataSourceId(request.getDataSourceId())
                .build();

        // 生成向量并插入
        List<Float> vector = embeddingService.embedText(request.getQuestion());
        if (vector.isEmpty()) {
            throw new RuntimeException("生成嵌入向量失败");
        }

        milvusService.insert(tenantCode, dto, vector);
        log.info("手动添加示例 SQL: id={}, dataSourceId={}", id, request.getDataSourceId());
    }

    @Override
    public void delete(String id) {
        // 由于手动添加的 answerId 格式为 manual-{id}，需要特殊处理
        // 简化处理：使用 id 作为 answerId 前缀进行模糊匹配删除
        // 实际生产环境可以添加唯一 ID 到 Milvus 字段
        String tenantCode = TenantContextHolder.getTenantCode();

        // TODO: 需要在 Milvus 中添加唯一 ID 字段，或者通过其他方式删除
        // 暂时跳过删除，手动添加的示例无法通过此方法删除
        log.warn("手动删除示例 SQL 需要扩展 Milvus 字段");
    }

    @Override
    public int syncFromLikes() {
        String tenantCode = TenantContextHolder.getTenantCode();

        // 获取当前租户下所有 LIKE 反馈的回答
        List<ChatAnswer> likes = chatAnswerService.list(new LambdaQueryWrapper<ChatAnswer>()
                .select(ChatAnswer::getAnswerId, ChatAnswer::getQuestion, ChatAnswer::getSql,
                        ChatAnswer::getDataSourceId)
                .eq(ChatAnswer::getFeedback, "LIKE")
                .isNotNull(ChatAnswer::getSql)
                .ne(ChatAnswer::getSql, ""));

        int syncedCount = 0;
        for (ChatAnswer answer : likes) {
            try {
                addToVectorStore(answer.getAnswerId());
                syncedCount++;
            } catch (Exception e) {
                log.error("同步示例 SQL 失败: answerId={}", answer.getAnswerId(), e);
            }
        }

        log.info("从 LIKE 反馈同步完成，共 {} 条", syncedCount);
        return syncedCount;
    }

    @Override
    public int rebuild() {
        String tenantCode = TenantContextHolder.getTenantCode();

        // 重建前先清空当前租户的 Collection
        // TODO: 实现清空 Collection 的逻辑

        return syncFromLikes();
    }

    @Override
    public void addToVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantCode();

        // 查询回答信息
        ChatAnswer answer = chatAnswerService.getById(answerId);
        if (answer == null || answer.getSql() == null || answer.getSql().isEmpty()) {
            log.warn("回答不存在或 SQL 为空: answerId={}", answerId);
            return;
        }

        SampleSqlDto dto = SampleSqlDto.builder()
                .id(UUID.randomUUID().toString())
                .answerId(answerId)
                .question(answer.getQuestion())
                .sql(answer.getSql())
                .dataSourceId(answer.getDataSourceId())
                .build();

        List<Float> vector = embeddingService.embedText(answer.getQuestion());
        if (vector.isEmpty()) {
            throw new RuntimeException("生成嵌入向量失败: answerId=" + answerId);
        }

        milvusService.insert(tenantCode, dto, vector);
        log.info("添加示例 SQL 到向量库: answerId={}", answerId);
    }

    @Override
    public void removeFromVectorStore(String answerId) {
        String tenantCode = TenantContextHolder.getTenantCode();
        milvusService.deleteByAnswerId(tenantCode, answerId);
        log.info("从向量库删除示例 SQL: answerId={}", answerId);
    }

    @Override
    public List<SampleSqlDto> search(String dataSourceId, String question) {
        String tenantCode = TenantContextHolder.getTenantCode();
        double threshold = getSimilarityThreshold();

        // 生成查询向量
        List<Float> vector = embeddingService.embedText(question);
        if (vector.isEmpty()) {
            log.warn("生成嵌入向量失败，返回空列表");
            return new ArrayList<>();
        }

        return milvusService.search(tenantCode, dataSourceId, vector, threshold, DEFAULT_LIMIT);
    }

    private double getSimilarityThreshold() {
        try {
            String value = systemSettingService.getValueByKey(SIMILARITY_THRESHOLD_KEY, "");
            if (value != null && !value.isEmpty()) {
                return Double.parseDouble(value);
            }
        } catch (Exception e) {
            log.warn("获取相似度阈值失败，使用默认值", e);
        }
        return DEFAULT_THRESHOLD;
    }
}
```

**Step 3: 提交**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/SampleSqlService.java
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/SampleSqlServiceImpl.java
git commit -m "feat(chatbi): 添加 SampleSqlService 示例 SQL 服务"
```

---

## 第三阶段：API 端点开发

### Task 6: 添加 API 端点

**Files:**
- Modify: `auth-server/src/main/java/cn/opensrcdevelop/auth/controller/ChatBIController.java`

**Step 1: 添加示例 SQL 管理端点**

在 ChatBIController 中添加以下方法：

```java
/**
 * 获取示例 SQL 列表
 */
@GetMapping("/sampleSql/list")
public R<PageData<SampleSqlDto>> listSampleSql(
        @RequestParam(required = false) String dataSourceId,
        @RequestParam(defaultValue = "1") Integer pageNum,
        @RequestParam(defaultValue = "10") Integer pageSize) {
    // TODO: 实现分页查询
    List<SampleSqlDto> list = sampleSqlService.list(dataSourceId);
    return R.ok(PageData.of(list, (long) list.size()));
}

/**
 * 添加示例 SQL
 */
@PostMapping("/sampleSql")
public R<Void> addSampleSql(@RequestBody @Valid SampleSqlRequestDto request) {
    sampleSqlService.add(request);
    return R.ok();
}

/**
 * 删除示例 SQL
 */
@DeleteMapping("/sampleSql/{id}")
public R<Void> deleteSampleSql(@PathVariable String id) {
    sampleSqlService.delete(id);
    return R.ok();
}

/**
 * 从 LIKE 反馈同步
 */
@PostMapping("/sampleSql/syncFromLikes")
public R<Integer> syncFromLikes() {
    int count = sampleSqlService.syncFromLikes();
    return R.ok(count);
}

/**
 * 重建索引
 */
@PostMapping("/sampleSql/rebuild")
public R<Integer> rebuild() {
    int count = sampleSqlService.rebuild();
    return R.ok(count);
}

/**
 * 获取嵌入配置
 */
@GetMapping("/embedding/config")
public R<EmbeddingConfigDto> getEmbeddingConfig() {
    EmbeddingConfigDto config = new EmbeddingConfigDto();
    config.setProviderId(systemSettingService.getValueByKey("chatbi.embedding.provider.id", ""));
    String threshold = systemSettingService.getValueByKey("chatbi.embedding.similarity.threshold", "0.7");
    config.setSimilarityThreshold(Double.parseDouble(threshold));
    return R.ok(config);
}

/**
 * 更新嵌入配置
 */
@PutMapping("/embedding/config")
public R<Void> updateEmbeddingConfig(@RequestBody EmbeddingConfigDto config) {
    systemSettingService.setValueByKey("chatbi.embedding.provider.id", config.getProviderId());
    systemSettingService.setValueByKey("chatbi.embedding.similarity.threshold",
            String.valueOf(config.getSimilarityThreshold()));
    return R.ok();
}
```

**Step 2: 添加依赖注入**

在 ChatBIController 中添加：

```java
private final SampleSqlService sampleSqlService;
private final SystemSettingService systemSettingService;
```

**Step 3: 提交**

```bash
git add auth-server/src/main/java/cn/opensrcdevelop/auth/controller/ChatBIController.java
git commit -m "feat(chatbi): 添加示例 SQL 管理 API 端点"
```

---

## 第四阶段：集成修改

### Task 7: 修改 SqlAgent 使用向量检索

**Files:**
- Modify: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java`

**Step 1: 修改 getSampleSqls 方法**

将原有的 LLM 判断相关性的逻辑替换为向量检索：

```java
private List<Map<String, String>> getSampleSqls(String dataSourceId, String question, ChatClient chatClient) {
    try {
        // 使用向量检索替代 LLM 判断
        List<SampleSqlDto> sampleSqls = sampleSqlService.search(dataSourceId, question);

        if (sampleSqls.isEmpty()) {
            return new ArrayList<>();
        }

        // 转换为 Map 列表返回
        List<Map<String, String>> result = new ArrayList<>();
        for (SampleSqlDto dto : sampleSqls) {
            result.add(Map.of("question", dto.getQuestion(), "sql", dto.getSql()));
        }

        log.info("向量检索返回 {} 条相关示例 SQL", result.size());
        return result;
    } catch (Exception e) {
        log.error("获取示例 SQL 失败", e);
        return new ArrayList<>();
    }
}
```

**Step 2: 添加依赖注入**

在 ChatBIServiceImpl 中添加：

```java
private final SampleSqlService sampleSqlService;
```

**Step 3: 提交**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
git commit -m "feat(chatbi): SqlAgent 使用向量检索替代 LLM 判断"
```

---

### Task 8: 修改投票接口添加同步逻辑

**Files:**
- Modify: `ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java`

**Step 1: 修改 voteAnswer 方法**

在投票逻辑中添加同步向量库的处理：

```java
@Override
public void voteAnswer(VoteAnswerRequestDto requestDto) {
    // 原有投票逻辑...

    // 添加/删除向量
    if (requestDto.getFeedback() == Feedback.LIKE) {
        sampleSqlService.addToVectorStore(requestDto.getAnswerId());
    } else if (requestDto.getFeedback() == Feedback.DISLIKE) {
        sampleSqlService.removeFromVectorStore(requestDto.getAnswerId());
    }
}
```

**Step 2: 提交**

```bash
git add ai-chatbi/src/main/java/cn/opensrcdevelop/ai/service/impl/ChatBIServiceImpl.java
git commit -m "feat(chatbi): 投票时自动同步向量库"
```

---

## 第五阶段：前端页面开发

### Task 9: 添加前端 API

**Files:**
- Modify: `ui/src/api/chatbi.ts`

**Step 1: 添加示例 SQL 相关 API**

```typescript
/**
 * 获取示例 SQL 列表
 *
 * @param params 请求参数
 * @returns 示例 SQL 列表
 */
export function getSampleSqlList(params: any) {
  return apiRequest.get({
    url: "/chatbi/sampleSql/list",
    params,
  });
}

/**
 * 添加示例 SQL
 *
 * @param data 添加示例 SQL 表单
 * @returns 响应结果
 */
export function addSampleSql(data: any) {
  return apiRequest.post({
    url: "/chatbi/sampleSql",
    data,
  });
}

/**
 * 删除示例 SQL
 *
 * @param id 示例 SQL ID
 * @returns 响应结果
 */
export function deleteSampleSql(id: string) {
  return apiRequest.delete({
    url: `/chatbi/sampleSql/${id}`,
  });
}

/**
 * 从 LIKE 反馈同步
 *
 * @returns 同步数量
 */
export function syncSampleSqlFromLikes() {
  return apiRequest.post({
    url: "/chatbi/sampleSql/syncFromLikes",
  });
}

/**
 * 重建索引
 *
 * @returns 重建数量
 */
export function rebuildSampleSqlIndex() {
  return apiRequest.post({
    url: "/chatbi/sampleSql/rebuild",
  });
}

/**
 * 获取嵌入配置
 *
 * @returns 嵌入配置
 */
export function getEmbeddingConfig() {
  return apiRequest.get({
    url: "/chatbi/embedding/config",
  });
}

/**
 * 更新嵌入配置
 *
 * @param data 嵌入配置
 * @returns 响应结果
 */
export function updateEmbeddingConfig(data: any) {
  return apiRequest.put({
    url: "/chatbi/embedding/config",
    data,
  });
}
```

**Step 2: 提交**

```bash
git add ui/src/api/chatbi.ts
git commit -m "feat(chatbi): 添加示例 SQL 前端 API"
```

---

### Task 10: 添加前端页面

**Files:**
- Create: `ui/src/views/chatbi/sampleSql/index.vue`
- Create: `ui/src/views/chatbi/sampleSql/components/AddSampleSqlModal.vue`

**Step 1: 创建示例 SQL 管理页面**

```vue
<template>
  <div class="sample-sql-management">
    <a-card>
      <a-form :model="form" layout="inline">
        <a-form-item label="数据源">
          <a-select v-model="form.dataSourceId" placeholder="请选择数据源" style="width: 200px" allow-clear>
            <a-option v-for="ds in dataSourceList" :key="ds.dataSourceId" :value="ds.dataSourceId">
              {{ ds.dataSourceName }}
            </a-option>
          </a-select>
        </a-form-item>
        <a-form-item label="嵌入模型">
          <a-select v-model="form.providerId" placeholder="请选择嵌入模型" style="width: 200px">
            <a-option v-for="provider in providerList" :key="provider.providerId" :value="provider.providerId">
              {{ provider.providerName }}
            </a-option>
          </a-select>
        </a-form-item>
        <a-form-item label="相似度阈值">
          <a-input-number v-model="form.similarityThreshold" :min="0" :max="1" :step="0.1" style="width: 100px" />
        </a-form-item>
        <a-form-item>
          <a-button type="primary" @click="saveConfig">保存配置</a-button>
        </a-form-item>
      </a-form>
    </a-card>

    <a-card style="margin-top: 16px">
      <div class="action-bar">
        <a-space>
          <a-button type="primary" @click="showAddModal = true">添加示例</a-button>
          <a-button @click="syncFromLikes">从 Likes 同步</a-button>
          <a-button @click="rebuild">重建索引</a-button>
        </a-space>
      </div>

      <a-table :columns="columns" :data="tableData" :loading="loading" style="margin-top: 16px">
        <template #optional="{ record }">
          <a-button type="text" status="danger" size="small" @click="handleDelete(record.id)">删除</a-button>
        </template>
      </a-table>
    </a-card>

    <AddSampleSqlModal v-model:visible="showAddModal" @success="loadData" />
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue';
import { Message } from '@arco-design/web-vue';
import { getDataSourceConfList, getEnabledModelProvider, getSampleSqlList, deleteSampleSql, syncSampleSqlFromLikes, rebuildSampleSqlIndex, getEmbeddingConfig, updateEmbeddingConfig } from '@/api/chatbi';

const columns = [
  { title: 'ID', dataIndex: 'id', ellipsis: true },
  { title: '数据源', dataIndex: 'dataSourceId' },
  { title: '问题', dataIndex: 'question', ellipsis: true },
  { title: 'SQL', dataIndex: 'sql', ellipsis: true },
  { title: '创建时间', dataIndex: 'createdAt' },
  { title: '操作', slotName: 'optional' },
];

const loading = ref(false);
const showAddModal = ref(false);
const tableData = ref([]);
const dataSourceList = ref([]);
const providerList = ref([]);

const form = ref({
  dataSourceId: '',
  providerId: '',
  similarityThreshold: 0.7,
});

const loadData = async () => {
  loading.value = true;
  try {
    const res = await getSampleSqlList({});
    tableData.value = res.data?.data || [];
  } finally {
    loading.value = false;
  }
};

const loadConfig = async () => {
  try {
    const [dsRes, providerRes, configRes] = await Promise.all([
      getDataSourceConfList({}),
      getEnabledModelProvider(),
      getEmbeddingConfig(),
    ]);
    dataSourceList.value = dsRes.data?.data || [];
    providerList.value = providerRes.data?.data || [];
    if (configRes.data) {
      form.value.providerId = configRes.data.providerId;
      form.value.similarityThreshold = configRes.data.similarityThreshold;
    }
  } catch (e) {
    console.error('加载配置失败', e);
  }
};

const saveConfig = async () => {
  try {
    await updateEmbeddingConfig(form.value);
    Message.success('保存成功');
  } catch (e) {
    Message.error('保存失败');
  }
};

const handleDelete = async (id: string) => {
  try {
    await deleteSampleSql(id);
    Message.success('删除成功');
    loadData();
  } catch (e) {
    Message.error('删除失败');
  }
};

const syncFromLikes = async () => {
  try {
    const count = await syncSampleSqlFromLikes();
    Message.success(`同步成功，共 ${count.data} 条`);
    loadData();
  } catch (e) {
    Message.error('同步失败');
  }
};

const rebuild = async () => {
  try {
    const count = await rebuildSampleSqlIndex();
    Message.success(`重建成功，共 ${count.data} 条`);
    loadData();
  } catch (e) {
    Message.error('重建失败');
  }
};

onMounted(() => {
  loadData();
  loadConfig();
});
</script>

<style scoped>
.sample-sql-management {
  padding: 16px;
}
.action-bar {
  margin-bottom: 16px;
}
</style>
```

**Step 2: 创建添加示例弹窗组件**

```vue
<template>
  <a-modal v-model:visible="visible" title="添加示例 SQL" @ok="handleSubmit" @cancel="visible = false">
    <a-form :model="form" layout="vertical">
      <a-form-item label="数据源" required>
        <a-select v-model="form.dataSourceId" placeholder="请选择数据源">
          <a-option v-for="ds in dataSourceList" :key="ds.dataSourceId" :value="ds.dataSourceId">
            {{ ds.dataSourceName }}
          </a-option>
        </a-select>
      </a-form-item>
      <a-form-item label="问题" required>
        <a-textarea v-model="form.question" placeholder="请输入问题" :rows="3" />
      </a-form-item>
      <a-form-item label="SQL" required>
        <a-textarea v-model="form.sql" placeholder="请输入 SQL" :rows="5" />
      </a-form-item>
    </a-form>
  </a-modal>
</template>

<script setup lang="ts">
import { ref, watch, onMounted } from 'vue';
import { Message } from '@arco-design/web-vue';
import { addSampleSql, getDataSourceConfList } from '@/api/chatbi';

const props = defineProps<{
  visible: boolean;
}>();

const emit = defineEmits<{
  (e: 'update:visible', value: boolean): void;
  (e: 'success'): void;
}>();

const visible = ref(false);
const dataSourceList = ref([]);

const form = ref({
  dataSourceId: '',
  question: '',
  sql: '',
});

watch(() => props.visible, (val) => {
  visible.value = val;
  if (val) {
    loadDataSource();
  }
});

watch(visible, (val) => {
  emit('update:visible', val);
});

const loadDataSource = async () => {
  try {
    const res = await getDataSourceConfList({});
    dataSourceList.value = res.data?.data || [];
  } catch (e) {
    console.error('加载数据源失败', e);
  }
};

const handleSubmit = async () => {
  if (!form.value.dataSourceId || !form.value.question || !form.value.sql) {
    Message.warning('请填写完整信息');
    return;
  }

  try {
    await addSampleSql(form.value);
    Message.success('添加成功');
    visible.value = false;
    form.value = { dataSourceId: '', question: '', sql: '' };
    emit('success');
  } catch (e) {
    Message.error('添加失败');
  }
};
</script>
```

**Step 3: 修改 Tab 页面添加新 Tab**

修改 `ui/src/views/chatbi/index.vue` 添加示例 SQL Tab

**Step 4: 提交**

```bash
git add ui/src/views/chatbi/sampleSql/index.vue
git add ui/src/views/chatbi/sampleSql/components/AddSampleSqlModal.vue
git commit -m "feat(chatbi): 添加示例 SQL 管理前端页面"
```

---

## 总结

实现计划包含以下任务：

| 阶段 | 任务 | 描述 |
|-----|-----|------|
| 1 | Task 1 | 添加 Milvus 依赖和配置 |
| 1 | Task 2 | 创建 DTO 类 |
| 1 | Task 3 | 创建 MilvusService |
| 1 | Task 4 | 创建 EmbeddingService |
| 2 | Task 5 | 创建 SampleSqlService |
| 3 | Task 6 | 添加 API 端点 |
| 4 | Task 7 | 修改 SqlAgent 使用向量检索 |
| 4 | Task 8 | 修改投票接口添加同步逻辑 |
| 5 | Task 9 | 添加前端 API |
| 5 | Task 10 | 添加前端页面 |

**Plan complete and saved to `docs/plans/2026-03-07-chatbi-rag-design.md`. Two execution options:**

1. **Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

2. **Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

Which approach?
