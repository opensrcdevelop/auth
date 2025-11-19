package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.dto.ChatAnswerResponseDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.mapper.ChatAnswerMapper;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.github.vertical_blank.sqlformatter.SqlFormatter;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
public class ChatAnswerServiceImpl extends ServiceImpl<ChatAnswerMapper, ChatAnswer> implements ChatAnswerService {

    /**
     * 获取回答的用户反馈
     *
     * @param answerId 回答ID
     * @return 回答的用户反馈
     */
    @Override
    public String getAnswerFeedback(String answerId) {
        return super.getById(answerId).getFeedback();
    }

    /**
     * 获取回答的 SQL
     *
     * @param answerId 回答ID
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
}
