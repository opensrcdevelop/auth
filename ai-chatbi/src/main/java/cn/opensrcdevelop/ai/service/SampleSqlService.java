package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.dto.SampleSqlRequestDto;
import cn.opensrcdevelop.common.response.PageData;
import java.util.List;

public interface SampleSqlService {

    /**
     * 获取示例 SQL 列表（支持按数据源筛选）
     *
     * @param dataSourceId
     *            数据源 ID（可选）
     * @return 示例 SQL 列表
     */
    List<SampleSqlDto> list(String dataSourceId);

    /**
     * 分页获取示例 SQL 列表
     *
     * @param dataSourceId
     *            数据源 ID（可选）
     * @param current
     *            当前页
     * @param size
     *            每页数量
     * @return 分页的示例 SQL 列表
     */
    PageData<SampleSqlDto> list(String dataSourceId, long current, long size);

    /**
     * 添加示例 SQL
     *
     * @param request
     *            请求
     */
    void add(SampleSqlRequestDto request);

    /**
     * 删除示例 SQL
     *
     * @param id
     *            ID
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
     * @param answerId
     *            回答 ID
     */
    void addToVectorStore(String answerId);

    /**
     * 根据 answerId 从向量库删除（投票 UNLIKE 时调用）
     *
     * @param answerId
     *            回答 ID
     */
    void removeFromVectorStore(String answerId);

    /**
     * RAG 检索相关示例 SQL（分页）
     *
     * @param dataSourceId
     *            数据源 ID
     * @param question
     *            问题
     * @param current
     *            当前页
     * @param size
     *            每页数量
     * @return 分页的相关示例 SQL 列表
     */
    PageData<SampleSqlDto> search(String dataSourceId, String question, long current, long size);
}
