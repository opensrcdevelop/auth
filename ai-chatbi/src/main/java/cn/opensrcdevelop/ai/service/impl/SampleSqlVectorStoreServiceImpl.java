package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlEmbeddingConfigDto;
import cn.opensrcdevelop.ai.service.SampleSqlVectorStoreService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import io.milvus.v2.client.MilvusClientV2;
import io.milvus.v2.common.DataType;
import io.milvus.v2.common.IndexParam;
import io.milvus.v2.service.collection.request.AddFieldReq;
import io.milvus.v2.service.collection.request.CreateCollectionReq;
import io.milvus.v2.service.collection.request.DropCollectionReq;
import io.milvus.v2.service.collection.request.HasCollectionReq;
import io.milvus.v2.service.vector.request.DeleteReq;
import io.milvus.v2.service.vector.request.InsertReq;
import io.milvus.v2.service.vector.request.QueryReq;
import io.milvus.v2.service.vector.request.SearchReq;
import io.milvus.v2.service.vector.request.data.FloatVec;
import io.milvus.v2.service.vector.response.QueryResp;
import io.milvus.v2.service.vector.response.SearchResp;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class SampleSqlVectorStoreServiceImpl implements SampleSqlVectorStoreService {

    private static final String COLLECTION_PREFIX = "sample_sql_";
    private static final String FIELD_ID = "id";
    private static final String FIELD_ANSWER_ID = "answer_id";
    private static final String FIELD_QUESTION = "question";
    private static final String FIELD_SQL = "sql";
    private static final String FIELD_DATA_SOURCE_ID = "data_source_id";
    private static final String FIELD_CREATED_AT = "created_at";
    private static final String FIELD_QUESTION_VECTOR = "question_vector";

    private final MilvusClientV2 milvusClient;
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
            boolean exists = milvusClient.hasCollection(HasCollectionReq.builder()
                    .collectionName(collectionName)
                    .build());
            if (!exists) {
                createCollection(collectionName);
            }
        } catch (Exception e) {
            throw new ServerException(e);
        }
    }

    /**
     * 插入示例 SQL 向量
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

        JsonObject data = new JsonObject();
        data.addProperty(FIELD_ID, sampleSqlDto.getId());
        data.addProperty(FIELD_ANSWER_ID, sampleSqlDto.getAnswerId());
        data.addProperty(FIELD_QUESTION, sampleSqlDto.getQuestion());
        data.addProperty(FIELD_SQL, sampleSqlDto.getSql());
        data.addProperty(FIELD_DATA_SOURCE_ID, sampleSqlDto.getDataSourceId());
        data.addProperty(FIELD_CREATED_AT, LocalDateTime.now()
                .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)));
        JsonArray questionVector = new JsonArray();
        CommonUtil.stream(vector).forEach(questionVector::add);
        data.add(FIELD_QUESTION_VECTOR, questionVector);

        milvusClient.insert(InsertReq.builder()
                .collectionName(collectionName)
                .data(List.of(data)).build());
    }

    /**
     * 删除示例 SQL 向量（根据回答 ID）
     *
     * @param tenantCode
     *            租户 Code
     * @param answerId
     *            回答 ID
     */
    @Override
    public void deleteByAnswerId(String tenantCode, String answerId) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        String deleteExpr = "answer_id == '" + answerId + "'";
        DeleteReq deleteReq = DeleteReq.builder()
                .collectionName(collectionName)
                .filter(deleteExpr)
                .build();

        milvusClient.delete(deleteReq);
    }

    /**
     * 删除示例 SQL 向量（根据 ID）
     *
     * @param tenantCode
     *            租户 Code
     * @param id
     *            ID
     */
    @Override
    public void deleteById(String tenantCode, String id) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        String deleteExpr = "id == '" + id + "'";
        DeleteReq deleteReq = DeleteReq.builder()
                .collectionName(collectionName)
                .filter(deleteExpr)
                .build();

        milvusClient.delete(deleteReq);
    }

    /**
     * 删除示例 SQL 向量（根据租户 Code）
     *
     * @param tenantCode
     *            租户 Code
     */
    @Override
    public void deleteAll(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        try {
            DropCollectionReq dropCollectionReq = DropCollectionReq.builder()
                    .collectionName(collectionName)
                    .build();
            milvusClient.dropCollection(dropCollectionReq);
        } catch (Exception e) {
            log.warn("删除 Collection {} 失败", collectionName);
        }
    }

    /**
     * 搜索示例 SQL 向量
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
     * @return 示例 SQL 向量分页数据
     */
    @Override
    public List<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector, double threshold,
            Integer topK) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        int limit = Objects.nonNull(topK) && topK > 0 ? topK : 100;

        SearchReq.SearchReqBuilder searchReqBuilder = SearchReq.builder()
                .collectionName(collectionName)
                .data(Collections.singletonList(new FloatVec(vector)))
                .annsField(FIELD_QUESTION_VECTOR)
                .outputFields(List.of(FIELD_ID, FIELD_ANSWER_ID, FIELD_QUESTION, FIELD_SQL, FIELD_DATA_SOURCE_ID,
                        FIELD_CREATED_AT))
                .limit(limit);

        if (StringUtils.isNotEmpty(dataSourceId)) {
            searchReqBuilder.filter("data_source_id == '" + dataSourceId + "'");
        }

        SearchResp searchResp = milvusClient.search(searchReqBuilder.build());
        List<SampleSqlDto> results = new ArrayList<>();

        List<List<SearchResp.SearchResult>> searchResults = searchResp.getSearchResults();
        for (List<SearchResp.SearchResult> resultsList : searchResults) {
            for (SearchResp.SearchResult result : resultsList) {
                double score = result.getScore();
                if (score < threshold) {
                    continue;
                }

                Map<String, Object> entity = result.getEntity();
                SampleSqlDto dto = SampleSqlDto.builder()
                        .id((String) entity.get(FIELD_ID))
                        .answerId((String) entity.get(FIELD_ANSWER_ID))
                        .question((String) entity.get(FIELD_QUESTION))
                        .sql((String) entity.get(FIELD_SQL))
                        .dataSourceId((String) entity.get(FIELD_DATA_SOURCE_ID))
                        .createdAt((String) entity.get(FIELD_CREATED_AT))
                        .score(score)
                        .build();
                results.add(dto);
            }
        }
        return results;
    }

    @Override
    public List<SampleSqlDto> list(String tenantCode, String dataSourceId, String question, long offset, int limit) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        // 构建过滤条件
        StringBuilder filterBuilder = new StringBuilder("id != ''");
        if (StringUtils.isNotEmpty(dataSourceId)) {
            filterBuilder.append(" and data_source_id == '").append(dataSourceId).append("'");
        }
        if (StringUtils.isNotEmpty(question)) {
            filterBuilder.append(" and question like \"%").append(question).append("%\"");
        }

        QueryReq.QueryReqBuilder queryReqBuilder = QueryReq.builder()
                .collectionName(collectionName)
                .filter(filterBuilder.toString())
                .outputFields(List.of(FIELD_ID, FIELD_ANSWER_ID, FIELD_QUESTION, FIELD_SQL, FIELD_DATA_SOURCE_ID,
                        FIELD_CREATED_AT))
                .offset(offset)
                .limit(limit);

        try {
            var queryResp = milvusClient.query(queryReqBuilder.build());
            List<SampleSqlDto> results = new ArrayList<>();

            List<QueryResp.QueryResult> queryResults = queryResp.getQueryResults();
            for (QueryResp.QueryResult queryResult : queryResults) {
                Map<String, Object> row = queryResult.getEntity();
                SampleSqlDto dto = SampleSqlDto.builder()
                        .id((String) row.get(FIELD_ID))
                        .answerId((String) row.get(FIELD_ANSWER_ID))
                        .question((String) row.get(FIELD_QUESTION))
                        .sql((String) row.get(FIELD_SQL))
                        .dataSourceId((String) row.get(FIELD_DATA_SOURCE_ID))
                        .createdAt((String) row.get(FIELD_CREATED_AT))
                        .build();
                results.add(dto);
            }

            // 按创建时间降序排列
            results.sort(Comparator.comparing(SampleSqlDto::getCreatedAt, Comparator.reverseOrder()));

            return results;
        } catch (Exception e) {
            log.error("查询示例 SQL 失败", e);
            return new ArrayList<>();
        }
    }

    @Override
    public long count(String tenantCode, String dataSourceId, String question) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        // 构建过滤条件
        StringBuilder filterBuilder = new StringBuilder("id != ''");
        if (StringUtils.isNotEmpty(dataSourceId)) {
            filterBuilder.append(" and data_source_id == '").append(dataSourceId).append("'");
        }
        if (StringUtils.isNotEmpty(question)) {
            filterBuilder.append(" and question like \"%").append(question).append("%\"");
        }

        QueryReq.QueryReqBuilder queryReqBuilder = QueryReq.builder()
                .collectionName(collectionName)
                .filter(filterBuilder.toString())
                .outputFields(List.of(FIELD_ID));

        try {
            var queryResp = milvusClient.query(queryReqBuilder.build());
            return queryResp.getQueryResults().size();
        } catch (Exception e) {
            log.error("统计示例 SQL 数量失败", e);
            return 0;
        }
    }

    /**
     * 删除 Collection
     *
     * @param tenantCode
     *            租户 Code
     */
    @Override
    public void removeCollection(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        milvusClient.dropCollection(DropCollectionReq.builder().collectionName(collectionName).build());
    }

    /**
     * 创建 Collection
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

        CreateCollectionReq.CollectionSchema schema = MilvusClientV2.CreateSchema();
        schema.setEnableDynamicField(true);

        // 主键字段
        schema.addField(
                AddFieldReq.builder()
                        .fieldName(FIELD_ID)
                        .dataType(DataType.VarChar)
                        .maxLength(64)
                        .isPrimaryKey(true)
                        .build());

        // 回答 ID 字段（允许 null）
        schema.addField(
                AddFieldReq.builder()
                        .fieldName(FIELD_ANSWER_ID)
                        .dataType(DataType.VarChar)
                        .maxLength(64)
                        .isNullable(true)
                        .build());

        // 问题字段
        schema.addField(
                AddFieldReq.builder()
                        .fieldName(FIELD_QUESTION)
                        .dataType(DataType.VarChar)
                        .build());

        // SQL 字段
        schema.addField(
                AddFieldReq.builder()
                        .fieldName(FIELD_SQL)
                        .dataType(DataType.VarChar)
                        .build());

        // 数据源 ID 字段
        schema.addField(
                AddFieldReq.builder()
                        .fieldName(FIELD_DATA_SOURCE_ID)
                        .dataType(DataType.VarChar)
                        .maxLength(64)
                        .build());

        // 创建时间字段
        schema.addField(
                AddFieldReq.builder()
                        .fieldName(FIELD_CREATED_AT)
                        .dataType(DataType.VarChar)
                        .maxLength(32)
                        .build());

        // 问题向量字段
        schema.addField(
                AddFieldReq.builder()
                        .fieldName(FIELD_QUESTION_VECTOR)
                        .dataType(DataType.FloatVector)
                        .dimension(embeddingConfig.getDimension())
                        .build());

        List<IndexParam> indexes = new ArrayList<>();
        indexes.add(IndexParam.builder()
                .fieldName(FIELD_QUESTION_VECTOR)
                .indexType(IndexParam.IndexType.AUTOINDEX)
                .metricType(IndexParam.MetricType.IP)
                .build());

        CreateCollectionReq createCollectionReq = CreateCollectionReq.builder()
                .collectionName(collectionName)
                .collectionSchema(schema)
                .indexParams(indexes)
                .build();
        milvusClient.createCollection(createCollectionReq);
        log.info("Collection {} created successfully", collectionName);
    }
}
