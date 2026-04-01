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
import cn.opensrcdevelop.common.exception.ServerException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chroma.vectorstore.ChromaApi;
import org.springframework.ai.chroma.vectorstore.ChromaApi.*;
import org.springframework.ai.chroma.vectorstore.ChromaApi.Collection;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@ConditionalOnProperty(name = "vectorstore.type", havingValue = "chroma")
@RequiredArgsConstructor
public class SampleSqlChromaVectorStoreServiceImpl implements SampleSqlVectorStoreService {

    private final ChromaApi chromaApi;
    private final ChromaConfigProperties chromaConfigProperties;
    private final SystemSettingService systemSettingService;

    /**
     * 创建 Collection（如果不存在）
     *
     * @param tenantCode
     *            租户 Code
     */
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
            createCollection(collectionName);
        }
    }

    /**
     * 插入向量
     *
     * @param tenantCode
     *            租户 Code
     * @param sampleSqlDto
     *            示例 SQL
     * @param vector
     *            向量
     */
    @Override
    public void insert(String tenantCode, SampleSqlDto sampleSqlDto, List<Float> vector) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        List<String> ids = List.of(sampleSqlDto.getId());
        List<float[]> embeddings = List.of(toFloatArray(vector));
        List<Map<String, Object>> metadata = List.of(buildMetadata(sampleSqlDto));
        List<String> documents = List.of(sampleSqlDto.getQuestion());

        chromaApi.upsertEmbeddings(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                getCollectionId(collectionName),
                new AddEmbeddingsRequest(ids, embeddings, metadata, documents));
    }

    /**
     * 删除向量
     *
     * @param tenantCode
     *            租户 Code
     * @param answerId
     *            回答 ID
     */
    @Override
    public void deleteByAnswerId(String tenantCode, String answerId) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        DeleteEmbeddingsRequest deleteRequest = new DeleteEmbeddingsRequest(
                null,
                Map.of(FIELD_ANSWER_ID, Map.of("$eq", answerId)));

        chromaApi.deleteEmbeddings(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                getCollectionId(collectionName),
                deleteRequest);
    }

    /**
     * 删除向量
     *
     * @param tenantCode
     *            租户 Code
     * @param id
     *            ID
     */
    @Override
    public void deleteById(String tenantCode, String id) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        DeleteEmbeddingsRequest deleteRequest = new DeleteEmbeddingsRequest(
                List.of(id),
                null);

        chromaApi.deleteEmbeddings(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                getCollectionId(collectionName),
                deleteRequest);
    }

    /**
     * 删除所有向量
     *
     * @param tenantCode
     *            租户 Code
     */
    @Override
    public void deleteAll(String tenantCode) {
        removeCollection(tenantCode);
    }

    /**
     * 搜索向量
     *
     * @param tenantCode
     *            租户 Code
     * @param dataSourceId
     *            数据源 ID
     * @param vector
     *            查询向量
     * @param threshold
     *            相似度阈值
     * @param topK
     *            返回的最大结果数
     * @return 搜索结果
     */
    @SuppressWarnings("java:S3776")
    @Override
    public List<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector, double threshold,
            Integer topK) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        int limit = Objects.nonNull(topK) && topK > 0 ? topK : 100;

        // 构建过滤条件
        List<Map<String, Object>> conditions = new ArrayList<>();

        if (StringUtils.isNotEmpty(dataSourceId)) {
            conditions.add(Map.of(FIELD_DATA_SOURCE_ID, Map.of("$eq", dataSourceId)));
        }

        Map<String, Object> where = null;
        if (!conditions.isEmpty()) {
            where = conditions.getFirst();
        }

        QueryRequest queryRequest = new QueryRequest(
                toFloatArray(vector),
                limit,
                where);

        QueryResponse queryResponse = chromaApi.queryCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                getCollectionId(collectionName),
                queryRequest);

        List<SampleSqlDto> results = new ArrayList<>();

        if (queryResponse != null) {
            List<List<String>> idsList = queryResponse.ids();
            List<List<Double>> distancesList = queryResponse.distances();
            List<List<Map<String, Object>>> metadataList = queryResponse.metadata();
            List<List<String>> documentsList = queryResponse.documents();

            if (!idsList.isEmpty()) {
                List<String> ids = idsList.getFirst();
                List<Double> distances = !distancesList.isEmpty()
                        ? distancesList.getFirst()
                        : null;
                List<Map<String, Object>> metadatas = !metadataList.isEmpty()
                        ? metadataList.getFirst()
                        : null;
                List<String> documents = !documentsList.isEmpty()
                        ? documentsList.getFirst()
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

    /**
     * 分页查询样本 SQL
     *
     * @param tenantCode
     *            租户 Code
     * @param dataSourceId
     *            数据源 ID
     * @param question
     *            问题关键词过滤
     * @param offset
     *            偏移量
     * @param limit
     *            每页数量
     * @return 分页查询结果列表
     */
    @SuppressWarnings("java:S3776")
    @Override
    public List<SampleSqlDto> list(String tenantCode, String dataSourceId, String question, long offset, int limit) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        // 构建过滤条件
        List<Map<String, Object>> conditions = new ArrayList<>();
        if (StringUtils.isNotEmpty(dataSourceId)) {
            conditions.add(Map.of(FIELD_DATA_SOURCE_ID, Map.of("$eq", dataSourceId)));
        }

        if (StringUtils.isNotEmpty(question)) {
            conditions.add(Map.of(FIELD_QUESTION, Map.of("$contains", question)));
        }

        // 构建 where 语句
        Map<String, Object> where;
        if (conditions.size() == 1) {
            where = conditions.getFirst();
        } else {
            where = Map.of("$and", conditions);
        }

        // 使用 queryCollection 获取结果（传入空向量表示获取所有）
        SampleSqlEmbeddingConfigDto embeddingConfig = systemSettingService.getSystemSetting(
                SystemSettingConstants.SAMPLE_SQL_EMBEDDING_CONFIG, SampleSqlEmbeddingConfigDto.class);
        float[] emptyVector = new float[embeddingConfig.getDimension()];
        QueryRequest queryRequest = new QueryRequest(emptyVector, limit, where);

        QueryResponse queryResponse = chromaApi.queryCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                getCollectionId(collectionName),
                queryRequest);

        List<SampleSqlDto> results = new ArrayList<>();

        if (queryResponse != null) {
            List<List<Map<String, Object>>> metadataList = queryResponse.metadata();
            List<List<String>> documentsList = queryResponse.documents();

            if (!metadataList.isEmpty()) {
                List<Map<String, Object>> metadatas = metadataList.getFirst();
                List<String> documents = !documentsList.isEmpty()
                        ? documentsList.getFirst()
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

    /**
     * 统计样本 SQL 数量
     *
     * @param tenantCode
     *            租户 Code
     * @param dataSourceId
     *            数据源 ID
     * @param question
     *            问题关键词过滤
     * @return 样本 SQL 数量
     */
    @Override
    public long count(String tenantCode, String dataSourceId, String question) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        Long count = chromaApi.countEmbeddings(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                getCollectionId(collectionName));

        return count != null ? count : 0;
    }

    /**
     * 删除样本 SQL 集合
     *
     * @param tenantCode
     *            租户 Code
     */
    @Override
    public void removeCollection(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        chromaApi.deleteCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionName);
    }

    /**
     * 创建样本 SQL 集合
     *
     * @param collectionName
     *            集合名称
     */
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

    /**
     * 获取集合ID
     *
     * @param collectionName
     *            集合名称
     * @return 集合ID
     */
    private String getCollectionId(String collectionName) {
        Collection collection = chromaApi.getCollection(
                chromaConfigProperties.getTenantName(),
                chromaConfigProperties.getDatabaseName(),
                collectionName);
        if (Objects.nonNull(collection)) {
            return collection.id();
        } else {
            throw new ServerException("Collection not found: " + collectionName);
        }
    }

    private float[] toFloatArray(List<Float> vector) {
        float[] arr = new float[vector.size()];
        for (int i = 0; i < vector.size(); i++) {
            arr[i] = vector.get(i);
        }
        return arr;
    }

    /**
     * 构建元数据
     *
     * @param sampleSqlDto
     *            示例 SQL DTO
     * @return 元数据
     */
    private Map<String, Object> buildMetadata(SampleSqlDto sampleSqlDto) {
        Map<String, Object> metadata = new HashMap<>();
        metadata.put(FIELD_ID, sampleSqlDto.getId());
        if (sampleSqlDto.getAnswerId() != null) {
            metadata.put(FIELD_ANSWER_ID, sampleSqlDto.getAnswerId());
        }
        metadata.put(FIELD_SQL, sampleSqlDto.getSql());
        metadata.put(FIELD_DATA_SOURCE_ID, sampleSqlDto.getDataSourceId());
        metadata.put(FIELD_QUESTION, sampleSqlDto.getQuestion());
        metadata.put(FIELD_CREATED_AT, LocalDateTime.now()
                .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)));
        return metadata;
    }
}
