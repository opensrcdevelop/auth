package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ChatAnswerResponseDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import com.baomidou.mybatisplus.extension.service.IService;

public interface ChatAnswerService extends IService<ChatAnswer> {

    String getAnswerFeedback(String answerId);

    ChatAnswerResponseDto getAnsweredSql(String answerId);
}
