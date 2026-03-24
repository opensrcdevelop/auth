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
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chroma.vectorstore.ChromaApi;
import org.springframework.ai.chroma.vectorstore.ChromaApi.AddEmbeddingsRequest;
import org.springframework.ai.chroma.vectorstore.ChromaApi.Collection;
import org.springframework.ai.chroma.vectorstore.ChromaApi.CreateCollectionRequest;
import org.springframework.ai.chroma.vectorstore.ChromaApi.DeleteEmbeddingsRequest;
import org.springframework.ai.chroma.vectorstore.ChromaApi.QueryRequest;
import org.springframework.ai.chroma.vectorstore.ChromaApi.QueryResponse;
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

        String collectionId = getCollectionId(tenantCode, collectionName);

        List<String> ids = List.of(sampleSqlDto.getId());
        List<float[]> embeddings = List.of(toFloatArray(vector));
        List<Map<String, Object>> metadatas = List.of(buildMetadata(sampleSqlDto));
        List<String> documents = List.of(sampleSqlDto.getQuestion());

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

        DeleteEmbeddingsRequest deleteRequest = new DeleteEmbeddingsRequest(
                null, // ids 为空
                Map.of("answer_id", Map.of("$eq", answerId))); // 使用 where 条件

        chromaApi.deleteEmbeddings(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                deleteRequest);
    }

    @Override
    public void deleteById(String tenantCode, String id) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        String collectionId = getCollectionId(tenantCode, collectionName);

        DeleteEmbeddingsRequest deleteRequest = new DeleteEmbeddingsRequest(
                List.of(id), // 使用 IDs
                null); // where 为空

        chromaApi.deleteEmbeddings(
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
                toFloatArray(vector),
                limit,
                where);

        QueryResponse queryResponse = chromaApi.queryCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                queryRequest);

        List<SampleSqlDto> results = new ArrayList<>();

        if (queryResponse != null) {
            List<List<String>> idsList = queryResponse.ids();
            List<List<Double>> distancesList = queryResponse.distances();
            List<List<Map<String, Object>>> metadataList = queryResponse.metadata();
            List<List<String>> documentsList = queryResponse.documents();

            if (idsList != null && !idsList.isEmpty()) {
                List<String> ids = idsList.get(0);
                List<Double> distances = distancesList != null && !distancesList.isEmpty()
                        ? distancesList.get(0)
                        : null;
                List<Map<String, Object>> metadatas = metadataList != null && !metadataList.isEmpty()
                        ? metadataList.get(0)
                        : null;
                List<String> documents = documentsList != null && !documentsList.isEmpty()
                        ? documentsList.get(0)
                        : null;

                for (int i = 0; i < ids.size(); i++) {
                    if (distances != null && i < distances.size()) {
                        // Chroma 使用余弦距离，similarity = 1 - distance
                        double distance = distances.get(i);
                        double similarity = 1 - distance;
                        if (similarity < threshold) {
                            continue;
                        }

                        if (metadatas != null && i < metadatas.size()) {
                            Map<String, Object> metadata = metadatas.get(i);
                            SampleSqlDto dto = SampleSqlDto.builder()
                                    .id((String) metadata.get(FIELD_ID))
                                    .answerId((String) metadata.get(FIELD_ANSWER_ID))
                                    .question(documents != null && i < documents.size() ? documents.get(i) : null)
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
        }

        return results;
    }

    @Override
    public List<SampleSqlDto> list(String tenantCode, String dataSourceId, String question, long offset, int limit) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        // Chroma 的 list API 使用 queryCollection 进行分页查询
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

        // 使用 queryCollection 获取结果（传入空向量表示获取所有）
        float[] emptyVector = new float[0];
        QueryRequest queryRequest = new QueryRequest(emptyVector, (int) limit, where);

        QueryResponse queryResponse = chromaApi.queryCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId,
                queryRequest);

        List<SampleSqlDto> results = new ArrayList<>();

        if (queryResponse != null) {
            List<List<Map<String, Object>>> metadataList = queryResponse.metadata();
            List<List<String>> documentsList = queryResponse.documents();

            if (metadataList != null && !metadataList.isEmpty()) {
                List<Map<String, Object>> metadatas = metadataList.get(0);
                List<String> documents = documentsList != null && !documentsList.isEmpty()
                        ? documentsList.get(0)
                        : null;

                for (int i = 0; i < metadatas.size(); i++) {
                    Map<String, Object> metadata = metadatas.get(i);
                    SampleSqlDto dto = SampleSqlDto.builder()
                            .id((String) metadata.get(FIELD_ID))
                            .answerId((String) metadata.get(FIELD_ANSWER_ID))
                            .question(documents != null && i < documents.size() ? documents.get(i) : null)
                            .sql((String) metadata.get(FIELD_SQL))
                            .dataSourceId((String) metadata.get(FIELD_DATA_SOURCE_ID))
                            .createdAt((String) metadata.get(FIELD_CREATED_AT))
                            .build();
                    results.add(dto);
                }

                // 按创建时间降序排列
                results.sort(Comparator.comparing(SampleSqlDto::getCreatedAt, Comparator.reverseOrder()));
            }
        }

        return results;
    }

    @Override
    public long count(String tenantCode, String dataSourceId, String question) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        String collectionId = getCollectionId(tenantCode, collectionName);

        Long count = chromaApi.countEmbeddings(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionId);

        return count != null ? count : 0;
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
                new CreateCollectionRequest(collectionName));
        log.info("Collection {} created successfully", collectionName);
    }

    private String getCollectionId(String tenantCode, String collectionName) {
        Collection collection = chromaApi.getCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionName);
        return collection.id();
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
