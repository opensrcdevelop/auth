package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.UserResponseRequestDto;
import cn.opensrcdevelop.ai.dto.VoteAnswerRequestDto;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

public interface ChatBIService {

    SseEmitter streamChatBI(ChatBIRequestDto requestDto);

    void voteAnswer(VoteAnswerRequestDto requestDto);

    /**
     * 处理用户对问题的回答
     * @param request 用户响应
     * @return SseEmitter
     */
    SseEmitter handleUserResponse(UserResponseRequestDto request);
}
