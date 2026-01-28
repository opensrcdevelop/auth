package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ChatAnswerResponseDto;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

public interface ChatAnswerService extends IService<ChatAnswer> {

    String getAnswerFeedback(String answerId);

    ChatAnswerResponseDto getAnsweredSql(String answerId);

    /**
     * 获取数据源下所有 LIKE 反馈的历史回答（问题和 SQL）
     *
     * @param dataSourceId
     *            数据源ID
     * @param limit
     *            返回数量限制
     * @return 历史问题和 SQL 列表
     */
    List<SampleSqlDto> getHistoricalAnswers(String dataSourceId, int limit);

    /**
     * 获取与当前问题相关的示例 SQL（用户反馈为 LIKE）
     *
     * @param dataSourceId
     *            数据源ID
     * @param question
     *            当前用户问题
     * @param limit
     *            返回数量限制
     * @return 示例 SQL 列表
     */
    List<SampleSqlDto> getSampleSqls(String dataSourceId, String question, int limit);
}
