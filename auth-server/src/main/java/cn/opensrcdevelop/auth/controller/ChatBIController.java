package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.service.ChatBIService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

@Tag(name = "Chat BI", description = "接口-Chat BI")
@RestController
@RequestMapping("/chatbi")
@RequiredArgsConstructor
public class ChatBIController {

    private final ChatBIService chatBIService;

    @Operation(summary = "流式对话", description = "流式对话")
    @PostMapping(path = "/chat/stream", produces = MediaType.TEXT_EVENT_STREAM_VALUE)
    public SseEmitter streamChatBI(@RequestBody ChatBIRequestDto requestDto) {
        return chatBIService.streamChatBI(requestDto);
    }
}
