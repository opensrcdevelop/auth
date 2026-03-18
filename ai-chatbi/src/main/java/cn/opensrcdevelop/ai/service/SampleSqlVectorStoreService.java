package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;

import java.util.List;

public interface SampleSqlVectorStoreService {

    /**
     * 创建 Collection（如果不存在）
     *
     * @param tenantCode
     *            租户 Code
     */
    void createCollectionIfNotExists(String tenantCode);

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
    void insert(String tenantCode, SampleSqlDto sampleSqlDto, List<Float> vector);

    /**
     * 根据 answerId 删除向量
     *
     * @param tenantCode
     *            租户 Code
     * @param answerId
     *            回答 ID
     */
    void deleteByAnswerId(String tenantCode, String answerId);

    /**
     * 搜索相似问题（支持分页）
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
     * @return 分页的相似问题-SQL 对列表
     */
    List<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector, double threshold,
            Integer topK);

    /**
     * 清空 Collection
     *
     * @param tenantCode
     *            租户 Code
     */
    void deleteAll(String tenantCode);

    /**
     * 分页查询所有向量
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
     * @return 向量列表
     */
    List<SampleSqlDto> list(String tenantCode, String dataSourceId, String question, long offset, int limit);

    /**
     * 根据 ID 删除向量
     *
     * @param tenantCode
     *            租户 Code
     * @param id
     *            ID
     */
    void deleteById(String tenantCode, String id);

    /**
     * 统计 Collection 中的向量数量
     *
     * @param tenantCode
     *            租户 Code
     * @param dataSourceId
     *            数据源 ID
     * @return 向量数量
     */
    long count(String tenantCode, String dataSourceId, String question);

    /**
     * 删除 Collection
     *
     * @param tenantCode
     *            租户 Code
     */
    void removeCollection(String tenantCode);
}
