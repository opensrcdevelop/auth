package cn.opensrcdevelop.ai.service;

import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.VoteChartRequestDto;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

public interface ChatBIService {

    SseEmitter streamChatBI(ChatBIRequestDto requestDto);

    SseEmitter streamAnalyzeData(ChatBIRequestDto requestDto);

    void voteChart(VoteChartRequestDto requestDto);
}
