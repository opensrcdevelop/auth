package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.VoteAnswerRequestDto;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

public interface ChatBIService {

    SseEmitter streamChatBI(ChatBIRequestDto requestDto);

    void voteAnswer(VoteAnswerRequestDto requestDto);
}
