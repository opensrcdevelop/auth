package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlEmbeddingConfigDto;
import cn.opensrcdevelop.ai.service.SampleSqlVectorStoreService;
import cn.opensrcdevelop.ai.vectorstore.chroma.ChromaConfigProperties;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chroma.api.ChromaApi;
import org.springframework.ai.chroma.api.ChromaApi.AddEmbeddingsRequest;
import org.springframework.ai.chroma.api.ChromaApi.Collection;
import org.springframework.ai.chroma.api.ChromaApi.DeleteRequest;
import org.springframework.ai.chroma.api.ChromaApi.GetRequest;
import org.springframework.ai.chroma.api.ChromaApi.QueryRequest;
import org.springframework.ai.chroma.api.ChromaApi.QueryResponse;
import org.springframework.ai.chroma.api.ChromaApi.TransformedVector;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class SampleSqlChromaVectorStoreServiceImpl implements SampleSqlVectorStoreService {

    private static final String COLLECTION_PREFIX = "sample_sql_";

    private static final String FIELD_ID = "id";

    private static final String FIELD_ANSWER_ID = "answer_id";

    private static final String FIELD_QUESTION = "question";

    private static final String FIELD_SQL = "sql";

    private static final String FIELD_DATA_SOURCE_ID = "data_source_id";

    private static final String FIELD_CREATED_AT = "created_at";

    private static final String FIELD_QUESTION_VECTOR = "question_vector";

    private final ChromaApi chromaApi;

    private final ChromaConfigProperties chromaConfigProperties;

    private final SystemSettingService systemSettingService;

    @Override
    public void createCollectionIfNotExists(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        try {
            Collection existing = chromaApi.getCollection(
                    chromaConfigProperties.getTenantName(),
                    chromaConfigProperties.getDatabaseName(),
                    collectionName);
            if (existing == null) {
                createCollection(collectionName);
            }
        } catch (Exception e) {
            // Collection 不存在，创建
            createCollection(collectionName);
        }
    }

    @Override
    public void insert(String tenantCode, SampleSqlDto sampleSqlDto, List<Float> vector) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        List<String> ids = List.of(sampleSqlDto.getId());
        List<float[]> embeddings = List.of(toFloatArray(vector));
        List<Map<String, Object>> metadatas = List.of(buildMetadata(sampleSqlDto));
        List<String> documents = List.of(sampleSqlDto.getQuestion());

        String collectionId = getCollectionId(tenantCode, collectionName);

        chromaApi.upsertEmbeddings(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                new AddEmbeddingsRequest(ids, embeddings, metadatas, documents));
    }

    @Override
    public void deleteByAnswerId(String tenantCode, String answerId) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        String collectionId = getCollectionId(tenantCode, collectionName);

        DeleteRequest deleteRequest = new DeleteRequest();
        deleteRequest.where(Map.of("answer_id", Map.of("$eq", answerId)));

        chromaApi.delete(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                deleteRequest);
    }

    @Override
    public void deleteById(String tenantCode, String id) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        String collectionId = getCollectionId(tenantCode, collectionName);

        DeleteRequest deleteRequest = new DeleteRequest();
        deleteRequest.where(Map.of("id", Map.of("$eq", id)));

        chromaApi.delete(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                deleteRequest);
    }

    @Override
    public void deleteAll(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        try {
            chromaApi.deleteCollection(
                    chromaConfigProperties.getTenantName(),
                    chromaConfigProperties.getDatabaseName(),
                    collectionName);
        } catch (Exception e) {
            log.warn("删除 Collection {} 失败", collectionName);
        }
    }

    @Override
    public List<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector, double threshold,
            Integer topK) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        int limit = Objects.nonNull(topK) && topK > 0 ? topK : 100;

        String collectionId = getCollectionId(tenantCode, collectionName);

        Map<String, Object> where = null;
        if (dataSourceId != null && !dataSourceId.isEmpty()) {
            where = Map.of("data_source_id", Map.of("$eq", dataSourceId));
        }

        QueryRequest queryRequest = new QueryRequest(
                List.of(toFloatArray(vector)),
                limit,
                where,
                null);

        QueryResponse queryResponse = chromaApi.queryCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                queryRequest);

        List<SampleSqlDto> results = new ArrayList<>();

        if (queryResponse != null && queryResponse.getResults() != null) {
            List<TransformedVector> vectors = queryResponse.getResults();
            for (TransformedVector tv : vectors) {
                if (tv.getDistances() != null && !tv.getDistances().isEmpty()) {
                    // Chroma 使用余弦距离，similarity = 1 - distance
                    double distance = tv.getDistances().get(0);
                    double similarity = 1 - distance;
                    if (similarity < threshold) {
                        continue;
                    }

                    Map<String, Object> metadata = tv.getMetadatas() != null && !tv.getMetadatas().isEmpty()
                            ? tv.getMetadatas().get(0)
                            : null;

                    if (metadata != null) {
                        SampleSqlDto dto = SampleSqlDto.builder()
                                .id((String) metadata.get(FIELD_ID))
                                .answerId((String) metadata.get(FIELD_ANSWER_ID))
                                .question(tv.getDocuments() != null && !tv.getDocuments().isEmpty()
                                        ? tv.getDocuments().get(0) : null)
                                .sql((String) metadata.get(FIELD_SQL))
                                .dataSourceId((String) metadata.get(FIELD_DATA_SOURCE_ID))
                                .createdAt((String) metadata.get(FIELD_CREATED_AT))
                                .score(similarity)
                                .build();
                        results.add(dto);
                    }
                }
            }
        }

        return results;
    }

    @Override
    public List<SampleSqlDto> list(String tenantCode, String dataSourceId, String question, long offset, int limit) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        String collectionId = getCollectionId(tenantCode, collectionName);

        // 构建过滤条件
        Map<String, Object> where = new HashMap<>();
        where.put("id", Map.of("$ne", ""));

        if (dataSourceId != null && !dataSourceId.isEmpty()) {
            where.put("data_source_id", Map.of("$eq", dataSourceId));
        }
        if (question != null && !question.isEmpty()) {
            where.put("question", Map.of("$contains", question));
        }

        GetRequest getRequest = new GetRequest();
        getRequest.where(where);
        getRequest.limit(limit);
        getRequest.offset(offset);

        var getResponse = chromaApi.get(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                getRequest);

        List<SampleSqlDto> results = new ArrayList<>();

        if (getResponse != null && getResponse.getResults() != null) {
            for (var item : getResponse.getResults()) {
                Map<String, Object> metadata = item.getMetadata();
                SampleSqlDto dto = SampleSqlDto.builder()
                        .id((String) metadata.get(FIELD_ID))
                        .answerId((String) metadata.get(FIELD_ANSWER_ID))
                        .question(item.getDocument())
                        .sql((String) metadata.get(FIELD_SQL))
                        .dataSourceId((String) metadata.get(FIELD_DATA_SOURCE_ID))
                        .createdAt((String) metadata.get(FIELD_CREATED_AT))
                        .build();
                results.add(dto);
            }

            // 按创建时间降序排列
            results.sort(Comparator.comparing(SampleSqlDto::getCreatedAt, Comparator.reverseOrder()));
        }

        return results;
    }

    @Override
    public long count(String tenantCode, String dataSourceId, String question) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        String collectionId = getCollectionId(tenantCode, collectionName);

        // 构建过滤条件
        Map<String, Object> where = new HashMap<>();
        where.put("id", Map.of("$ne", ""));

        if (dataSourceId != null && !dataSourceId.isEmpty()) {
            where.put("data_source_id", Map.of("$eq", dataSourceId));
        }
        if (question != null && !question.isEmpty()) {
            where.put("question", Map.of("$contains", question));
        }

        GetRequest getRequest = new GetRequest();
        getRequest.where(where);
        getRequest.limit(1);

        try {
            var getResponse = chromaApi.get(
                    chromaConfigProperties.getTenantName(),
                    chromaConfigProperties.getDatabaseName(),
                    collectionId,
                    getRequest);
            // Chroma 没有 count API，通过 total 来获取
            if (getResponse != null && getResponse.getResults() != null) {
                return getResponse.getResults().size();
            }
        } catch (Exception e) {
            log.error("统计示例 SQL 数量失败", e);
        }
        return 0;
    }

    @Override
    public void removeCollection(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        chromaApi.deleteCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionName);
    }

    private void createCollection(String collectionName) {
        SampleSqlEmbeddingConfigDto embeddingConfig = systemSettingService.getSystemSetting(
                SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, SampleSqlEmbeddingConfigDto.class);
        if (Objects.isNull(embeddingConfig) || Objects.isNull(embeddingConfig.getDimension())) {
            throw new BizException(MessageConstants.AI_SAMPLE_SQL_MSG_1000);
        }

        chromaApi.createCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                new ChromaApi.CreateCollectionRequest(collectionName, embeddingConfig.getDimension()));
        log.info("Collection {} created successfully", collectionName);
    }

    private String getCollectionId(String tenantCode, String collectionName) {
        Collection collection = chromaApi.getCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionName);
        return collection.getId();
    }

    private float[] toFloatArray(List<Float> vector) {
        float[] arr = new float[vector.size()];
        for (int i = 0; i < vector.size(); i++) {
            arr[i] = vector.get(i);
        }
        return arr;
    }

    private Map<String, Object> buildMetadata(SampleSqlDto sampleSqlDto) {
        Map<String, Object> metadata = new HashMap<>();
        metadata.put(FIELD_ID, sampleSqlDto.getId());
        if (sampleSqlDto.getAnswerId() != null) {
            metadata.put(FIELD_ANSWER_ID, sampleSqlDto.getAnswerId());
        }
        metadata.put(FIELD_SQL, sampleSqlDto.getSql());
        metadata.put(FIELD_DATA_SOURCE_ID, sampleSqlDto.getDataSourceId());
        metadata.put(FIELD_CREATED_AT, LocalDateTime.now()
                .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)));
        return metadata;
    }
}
