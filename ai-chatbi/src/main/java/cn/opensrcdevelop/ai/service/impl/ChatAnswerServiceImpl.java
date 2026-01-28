package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.ChatAnswerResponseDto;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.mapper.ChatAnswerMapper;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.vertical_blank.sqlformatter.SqlFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

@Service
public class ChatAnswerServiceImpl extends ServiceImpl<ChatAnswerMapper, ChatAnswer> implements ChatAnswerService {

    /**
     * 获取回答的用户反馈
     *
     * @param answerId
     *            回答ID
     * @return 回答的用户反馈
     */
    @Override
    public String getAnswerFeedback(String answerId) {
        return super.getById(answerId).getFeedback();
    }

    /**
     * 获取回答的 SQL
     *
     * @param answerId
     *            回答ID
     * @return 回答的 SQL
     */
    @Override
    public ChatAnswerResponseDto getAnsweredSql(String answerId) {
        ChatAnswer chatAnswer = super.getOne(Wrappers.<ChatAnswer>lambdaQuery()
                .select(ChatAnswer::getAnswerId, ChatAnswer::getSql)
                .eq(ChatAnswer::getAnswerId, answerId));
        if (Objects.isNull(chatAnswer)) {
            return ChatAnswerResponseDto.builder()
                    .answerId(answerId)
                    .sql("")
                    .build();
        }

        String sql = chatAnswer.getSql();
        if (StringUtils.isNotEmpty(sql)) {
            sql = SqlFormatter.standard().format(sql);
        }

        return ChatAnswerResponseDto.builder()
                .answerId(chatAnswer.getAnswerId())
                .sql(sql)
                .build();
    }

    /**
     * 获取数据源下所有 LIKE 反馈的历史回答（问题和 SQL）
     *
     * @param dataSourceId
     *            数据源ID
     * @param limit
     *            返回数量限制
     * @return 历史问题和 SQL 列表
     */
    @Override
    public List<SampleSqlDto> getHistoricalAnswers(String dataSourceId, int limit) {
        if (StringUtils.isBlank(dataSourceId)) {
            return new ArrayList<>();
        }

        List<ChatAnswer> likedAnswers = super.list(Wrappers.<ChatAnswer>lambdaQuery()
                .select(ChatAnswer::getAnswerId, ChatAnswer::getQuestion, ChatAnswer::getSql)
                .eq(ChatAnswer::getDataSourceId, dataSourceId)
                .eq(ChatAnswer::getFeedback, "LIKE")
                .isNotNull(ChatAnswer::getSql)
                .ne(ChatAnswer::getSql, "")
                .last("LIMIT " + limit));

        if (likedAnswers.isEmpty()) {
            return new ArrayList<>();
        }

        List<SampleSqlDto> result = new ArrayList<>();
        for (ChatAnswer answer : likedAnswers) {
            String sql = answer.getSql();
            if (StringUtils.isNotEmpty(sql)) {
                sql = SqlFormatter.standard().format(sql);
            }
            result.add(SampleSqlDto.builder()
                    .question(answer.getQuestion())
                    .sql(sql)
                    .build());
        }

        return result;
    }

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
    @Override
    public List<SampleSqlDto> getSampleSqls(String dataSourceId, String question, int limit) {
        // 该方法已废弃，由 Agent 判断关联性
        // 使用 getHistoricalAnswers 获取历史回答，由 ChatAgent 判断相关性
        return getHistoricalAnswers(dataSourceId, limit);
    }

    /**
     * 根据 answerId 列表查询对应的 SQL
     *
     * @param answerIds
     *            回答ID列表
     * @return 示例 SQL 列表
     */
    @Override
    public List<SampleSqlDto> getSqlsByAnswerIds(List<String> answerIds) {
        if (answerIds == null || answerIds.isEmpty()) {
            return new ArrayList<>();
        }

        List<ChatAnswer> answers = super.list(Wrappers.<ChatAnswer>lambdaQuery()
                .select(ChatAnswer::getQuestion, ChatAnswer::getSql)
                .in(ChatAnswer::getAnswerId, answerIds));

        if (answers.isEmpty()) {
            return new ArrayList<>();
        }

        // 按 answerId 列表的顺序返回结果
        List<SampleSqlDto> result = new ArrayList<>();
        for (String answerId : answerIds) {
            for (ChatAnswer answer : answers) {
                if (answerId.equals(answer.getAnswerId())) {
                    String sql = answer.getSql();
                    if (StringUtils.isNotEmpty(sql)) {
                        sql = SqlFormatter.standard().format(sql);
                    }
                    result.add(SampleSqlDto.builder()
                            .question(answer.getQuestion())
                            .sql(sql)
                            .build());
                    break;
                }
            }
        }

        return result;
    }
}
