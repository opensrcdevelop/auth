package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.config.VectorStoreConfig;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.service.SampleSqlVectorStoreService;
import cn.opensrcdevelop.common.response.PageData;
import com.google.gson.JsonObject;
import io.milvus.v2.client.ConnectConfig;
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
import io.milvus.v2.service.vector.response.DeleteResp;
import io.milvus.v2.service.vector.response.InsertResp;
import io.milvus.v2.service.vector.response.SearchResp;
import jakarta.annotation.PostConstruct;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class SampleSqlVectorStoreServiceImpl implements SampleSqlVectorStoreService {

    private static final String COLLECTION_PREFIX = "sample_sql_";
    private static final int VECTOR_DIMENSION = 1536;

    private final VectorStoreConfig vectorStoreConfig;
    private MilvusClientV2 milvusClient;

    @PostConstruct
    public void init() {
        ConnectConfig connectConfig = ConnectConfig.builder()
                .uri("http://" + vectorStoreConfig.getHost() + ":" + vectorStoreConfig.getPort())
                .build();
        milvusClient = new MilvusClientV2(connectConfig);
        log.info("Milvus client initialized: {}:{}", vectorStoreConfig.getHost(), vectorStoreConfig.getPort());
    }

    @Override
    public void createCollectionIfNotExists(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        try {
            HasCollectionReq hasCollectionReq = HasCollectionReq.builder()
                    .collectionName(collectionName)
                    .build();
            boolean exists = milvusClient.hasCollection(hasCollectionReq).booleanValue();
            if (!exists) {
                createCollection(collectionName);
            }
        } catch (Exception e) {
            createCollection(collectionName);
        }
    }

    private void createCollection(String collectionName) {
        CreateCollectionReq.CollectionSchema schema = milvusClient.createSchema();
        schema.setEnableDynamicField(true);

        schema.addField(AddFieldReq.builder()
                .fieldName("id")
                .dataType(DataType.VarChar)
                .maxLength(64)
                .isPrimaryKey(true)
                .build());

        schema.addField(AddFieldReq.builder()
                .fieldName("answer_id")
                .dataType(DataType.VarChar)
                .maxLength(64)
                .build());

        schema.addField(AddFieldReq.builder()
                .fieldName("question")
                .dataType(DataType.VarChar)
                .maxLength(2000)
                .build());

        schema.addField(AddFieldReq.builder()
                .fieldName("sql")
                .dataType(DataType.VarChar)
                .maxLength(4000)
                .build());

        schema.addField(AddFieldReq.builder()
                .fieldName("data_source_id")
                .dataType(DataType.VarChar)
                .maxLength(64)
                .build());

        schema.addField(AddFieldReq.builder()
                .fieldName("question_vector")
                .dataType(DataType.FloatVector)
                .dimension(VECTOR_DIMENSION)
                .build());

        schema.addField(AddFieldReq.builder()
                .fieldName("created_at")
                .dataType(DataType.VarChar)
                .maxLength(32)
                .build());

        List<IndexParam> indexes = new ArrayList<>();
        indexes.add(IndexParam.builder()
                .fieldName("question_vector")
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

    @Override
    public void insert(String tenantCode, SampleSqlDto sampleSqlDto, List<Float> vector) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        JsonObject data = new JsonObject();
        data.addProperty("id", sampleSqlDto.getId());
        data.addProperty("answer_id", sampleSqlDto.getAnswerId());
        data.addProperty("question", sampleSqlDto.getQuestion());
        data.addProperty("sql", sampleSqlDto.getSql());
        data.addProperty("data_source_id", sampleSqlDto.getDataSourceId());
        data.addProperty("created_at", LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")));

        InsertReq insertReq = InsertReq.builder()
                .collectionName(collectionName)
                .data(Collections.singletonList(data))
                .build();

        InsertResp insertResp = milvusClient.insert(insertReq);
        log.info("Inserted sample SQL: id={}, answerId={}", sampleSqlDto.getId(), sampleSqlDto.getAnswerId());
    }

    @Override
    public void deleteByAnswerId(String tenantCode, String answerId) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        String deleteExpr = "answer_id == '" + answerId + "'";
        DeleteReq deleteReq = DeleteReq.builder()
                .collectionName(collectionName)
                .filter(deleteExpr)
                .build();

        DeleteResp deleteResp = milvusClient.delete(deleteReq);
        log.info("Deleted sample SQL by answerId: {}", answerId);
    }

    @Override
    public void deleteById(String tenantCode, String id) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        String deleteExpr = "id == '" + id + "'";
        DeleteReq deleteReq = DeleteReq.builder()
                .collectionName(collectionName)
                .filter(deleteExpr)
                .build();

        milvusClient.delete(deleteReq);
        log.info("Deleted sample SQL by id: {}", id);
    }

    @Override
    public void deleteAll(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;

        try {
            DropCollectionReq dropCollectionReq = DropCollectionReq.builder()
                    .collectionName(collectionName)
                    .build();
            milvusClient.dropCollection(dropCollectionReq);
            log.info("Dropped collection: {}", collectionName);
        } catch (Exception e) {
            log.warn("Collection {} does not exist or already dropped", collectionName);
        }
    }

    @Override
    public PageData<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector,
            double threshold, long current, long size) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        FloatVec queryVector = new FloatVec(vector);

        // 使用 Milvus 原生分页：offset = (current - 1) * size
        long offset = (current - 1) * size;
        int searchLimit = (int) size;
        SearchReq searchReq = SearchReq.builder()
                .collectionName(collectionName)
                .data(Collections.singletonList(queryVector))
                .annsField("question_vector")
                .filter("data_source_id == '" + dataSourceId + "'")
                .offset(offset)
                .topK(searchLimit)
                .outputFields(Arrays.asList("id", "answer_id", "question", "sql", "data_source_id", "created_at"))
                .build();

        SearchResp searchResp = milvusClient.search(searchReq);
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
                        .id((String) entity.get("id"))
                        .answerId((String) entity.get("answer_id"))
                        .question((String) entity.get("question"))
                        .sql((String) entity.get("sql"))
                        .dataSourceId((String) entity.get("data_source_id"))
                        .createdAt((String) entity.get("created_at"))
                        .score(score)
                        .build();
                results.add(dto);
            }
        }

        // 由于 Milvus 不直接返回总数，这里使用返回结果数作为参考
        // 实际总数需要通过其他方式获取，这里简单处理
        long total = results.isEmpty() ? 0 : (current * size + results.size());
        long pages = (total + size - 1) / size;

        PageData<SampleSqlDto> pageData = new PageData<>();
        pageData.setCurrent(current);
        pageData.setSize(size);
        pageData.setTotal(total);
        pageData.setPages(pages);
        pageData.setList(results);

        log.info("Search returned {} results (threshold={}, current={}, size={})",
                results.size(), threshold, current, size);
        return pageData;
    }

    @Override
    public List<SampleSqlDto> list(String tenantCode, int offset, int limit) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        QueryReq queryReq = QueryReq.builder()
                .collectionName(collectionName)
                .filter("id != ''")
                .offset(offset)
                .limit(limit)
                .outputFields(Arrays.asList("id", "answer_id", "question", "sql", "data_source_id", "created_at"))
                .build();

        try {
            var queryResp = milvusClient.query(queryReq);
            List<SampleSqlDto> results = new ArrayList<>();

            List<io.milvus.v2.service.vector.response.QueryResp.QueryResult> queryResults = queryResp.getQueryResults();
            for (io.milvus.v2.service.vector.response.QueryResp.QueryResult queryResult : queryResults) {
                Map<String, Object> row = queryResult.getEntity();
                SampleSqlDto dto = SampleSqlDto.builder()
                        .id((String) row.get("id"))
                        .answerId((String) row.get("answer_id"))
                        .question((String) row.get("question"))
                        .sql((String) row.get("sql"))
                        .dataSourceId((String) row.get("data_source_id"))
                        .createdAt((String) row.get("created_at"))
                        .build();
                results.add(dto);
            }

            log.info("List returned {} results (offset={}, limit={})", results.size(), offset, limit);
            return results;
        } catch (Exception e) {
            log.error("查询示例 SQL 失败", e);
            return new ArrayList<>();
        }
    }

    @Override
    public PageData<SampleSqlDto> list(String tenantCode, long current, long size) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        // 使用 Milvus 原生分页
        long offset = (current - 1) * size;
        int limit = (int) size;

        QueryReq queryReq = QueryReq.builder()
                .collectionName(collectionName)
                .filter("id != ''")
                .offset(offset)
                .limit(limit)
                .outputFields(Arrays.asList("id", "answer_id", "question", "sql", "data_source_id", "created_at"))
                .build();

        try {
            var queryResp = milvusClient.query(queryReq);
            List<SampleSqlDto> results = new ArrayList<>();

            List<io.milvus.v2.service.vector.response.QueryResp.QueryResult> queryResults = queryResp.getQueryResults();
            for (io.milvus.v2.service.vector.response.QueryResp.QueryResult queryResult : queryResults) {
                Map<String, Object> row = queryResult.getEntity();
                SampleSqlDto dto = SampleSqlDto.builder()
                        .id((String) row.get("id"))
                        .answerId((String) row.get("answer_id"))
                        .question((String) row.get("question"))
                        .sql((String) row.get("sql"))
                        .dataSourceId((String) row.get("data_source_id"))
                        .createdAt((String) row.get("created_at"))
                        .build();
                results.add(dto);
            }

            // 估算总数（当前页结果数 + 之前页数）
            long total = results.isEmpty() ? 0 : (current - 1) * size + results.size();
            long pages = (total + size - 1) / size;

            PageData<SampleSqlDto> pageData = new PageData<>();
            pageData.setCurrent(current);
            pageData.setSize(size);
            pageData.setTotal(total);
            pageData.setPages(pages);
            pageData.setList(results);

            log.info("List returned {} results (current={}, size={})", results.size(), current, size);
            return pageData;
        } catch (Exception e) {
            log.error("查询示例 SQL 失败", e);
            PageData<SampleSqlDto> emptyPageData = new PageData<>();
            emptyPageData.setCurrent(current);
            emptyPageData.setSize(size);
            emptyPageData.setTotal(0L);
            emptyPageData.setPages(0L);
            emptyPageData.setList(new ArrayList<>());
            return emptyPageData;
        }
    }

    @Override
    public long count(String tenantCode) {
        String collectionName = COLLECTION_PREFIX + tenantCode;
        createCollectionIfNotExists(tenantCode);

        QueryReq queryReq = QueryReq.builder()
                .collectionName(collectionName)
                .filter("id != ''")
                .outputFields(Collections.singletonList("id"))
                .limit(1)
                .build();

        try {
            var queryResp = milvusClient.query(queryReq);
            // Milvus v2 不支持直接 count，需要用其他方式
            // 这里简单返回 -1 表示未知，实际可用 getCollectionStats
            return -1;
        } catch (Exception e) {
            log.error("统计示例 SQL 数量失败", e);
            return 0;
        }
    }
}
