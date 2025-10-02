package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.mapper.ChatAnswerMapper;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

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
}
