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
     * @param sampleSqlDto 示例 SQL
     * @param vector 向量
     */
    void insert(String tenantCode, SampleSqlDto sampleSqlDto, List<Float> vector);

    /**
     * 根据 answerId 删除向量
     *
     * @param tenantCode 租户 Code
     * @param answerId 回答 ID
     */
    void deleteByAnswerId(String tenantCode, String answerId);

    /**
     * 搜索相似问题
     *
     * @param tenantCode 租户 Code
     * @param dataSourceId 数据源 ID
     * @param vector 查询向量
     * @param threshold 相似度阈值
     * @param limit 返回数量
     * @return 相似的问题-SQL 对列表
     */
    List<SampleSqlDto> search(String tenantCode, String dataSourceId, List<Float> vector,
            double threshold, int limit);

    /**
     * 清空 Collection
     *
     * @param tenantCode 租户 Code
     */
    void deleteAll(String tenantCode);

    /**
     * 查询所有向量（不分数据源筛选）
     *
     * @param tenantCode 租户 Code
     * @return 所有向量列表
     */
    List<SampleSqlDto> listAll(String tenantCode);

    /**
     * 根据 ID 删除向量
     *
     * @param tenantCode 租户 Code
     * @param id ID
     */
    void deleteById(String tenantCode, String id);
}
