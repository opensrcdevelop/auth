package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.config.MilvusConfig;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.service.MilvusService;
import io.milvus.client.MilvusServiceClient;
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
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

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
    public void deleteById(String tenantCode, String id) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        String deleteExpr = "id == '" + id + "'";
        DeleteParam deleteParam = DeleteParam.newBuilder()
                .withCollectionName(collectionName)
                .withExpr(deleteExpr)
                .build();

        milvusServiceClient.delete(deleteParam);
        log.info("Deleted sample SQL by id: {}", id);
    }

    @Override
    public void deleteAll(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        // 删除所有数据
        String deleteExpr = "id != ''";
        DeleteParam deleteParam = DeleteParam.newBuilder()
                .withCollectionName(collectionName)
                .withExpr(deleteExpr)
                .build();

        milvusServiceClient.delete(deleteParam);
        log.info("Deleted all sample SQL from collection: {}", collectionName);
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

    @Override
    public List<SampleSqlDto> listAll(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        // 确保 Collection 存在
        createCollectionIfNotExists(tenantCode);

        // 查询所有数据
        List<String> outputFields = Arrays.asList("id", "answer_id", "question", "sql", "data_source_id", "created_at");

        QueryParam queryParam = QueryParam.newBuilder()
                .withCollectionName(collectionName)
                .withOutputFields(outputFields)
                .withExpr("id != ''")
                .build();

        try {
            QueryResultsWrapper wrapper = milvusServiceClient.query(queryParam).getData();
            List<SampleSqlDto> results = new ArrayList<>();

            for (QueryResultsWrapper.RowWrapper row : wrapper.getRowWrapper()) {
                SampleSqlDto dto = SampleSqlDto.builder()
                        .id(row.getString("id"))
                        .answerId(row.getString("answer_id"))
                        .question(row.getString("question"))
                        .sql(row.getString("sql"))
                        .dataSourceId(row.getString("data_source_id"))
                        .createdAt(row.getString("created_at"))
                        .build();
                results.add(dto);
            }

            log.info("List all returned {} results", results.size());
            return results;
        } catch (Exception e) {
            log.error("查询所有示例 SQL 失败", e);
            return new ArrayList<>();
        }
    }
}
