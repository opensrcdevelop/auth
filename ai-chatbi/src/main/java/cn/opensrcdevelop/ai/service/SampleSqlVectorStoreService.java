package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.common.response.PageData;
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
     * @param current
     *            当前页
     * @param size
     *            每页数量
     * @return 分页的相似问题-SQL 对列表
     */
    PageData<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector,
            double threshold, long current, long size);

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
     * @param offset
     *            偏移量
     * @param limit
     *            每页数量
     * @return 向量列表
     */
    List<SampleSqlDto> list(String tenantCode, int offset, int limit);

    /**
     * 分页查询所有向量（返回 PageData）
     *
     * @param tenantCode
     *            租户 Code
     * @param current
     *            当前页
     * @param size
     *            每页数量
     * @return 分页的向量列表
     */
    PageData<SampleSqlDto> list(String tenantCode, long current, long size);

    /**
     * 查询总数
     *
     * @param tenantCode
     *            租户 Code
     * @return 总数
     */
    long count(String tenantCode);

    /**
     * 根据 ID 删除向量
     *
     * @param tenantCode
     *            租户 Code
     * @param id
     *            ID
     */
    void deleteById(String tenantCode, String id);
}
