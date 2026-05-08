package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.ChatAnswerResponseDto;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.entity.ChatHistory;
import cn.opensrcdevelop.ai.mapper.ChatAnswerMapper;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import cn.opensrcdevelop.ai.service.ChatHistoryService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.vertical_blank.sqlformatter.SqlFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import jakarta.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

@Service
public class ChatAnswerServiceImpl extends ServiceImpl<ChatAnswerMapper, ChatAnswer> implements ChatAnswerService {

    @Resource
    @Lazy
    private ChatHistoryService chatHistoryService;

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
                .select(ChatAnswer::getAnswerId, ChatAnswer::getQuestion, ChatAnswer::getSql)
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

    @Override
    public ChatAnswerResponseDto getCurrentUserAnswer(String answerId) {
        ChatAnswer chatAnswer = super.getOne(Wrappers.<ChatAnswer>lambdaQuery()
                .select(ChatAnswer::getAnswerId, ChatAnswer::getAnswer, ChatAnswer::getChatId)
                .eq(ChatAnswer::getAnswerId, answerId));
        if (Objects.isNull(chatAnswer)) {
            return ChatAnswerResponseDto.builder().answerId(answerId).answer("").build();
        }

        ChatHistory chatHistory = chatHistoryService.getOne(Wrappers.<ChatHistory>lambdaQuery()
                .select(ChatHistory::getUserId).eq(ChatHistory::getChatId, chatAnswer.getChatId()));
        if (chatHistory == null) {
            return ChatAnswerResponseDto.builder().answerId(answerId).answer("").build();
        }

        if (!chatHistory.getUserId().equals(AuthUtil.getCurrentUserId())) {
            return ChatAnswerResponseDto.builder().answerId(answerId).answer("").build();
        }

        return ChatAnswerResponseDto.builder().answerId(chatAnswer.getAnswerId()).answer(chatAnswer.getAnswer())
                .build();
    }
}
