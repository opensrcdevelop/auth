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
     * @param question 问题
     * @return 相关示例 SQL 列表
     */
    List<SampleSqlDto> search(String dataSourceId, String question);
}
