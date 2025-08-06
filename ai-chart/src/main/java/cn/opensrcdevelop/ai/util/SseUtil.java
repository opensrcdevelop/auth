package cn.opensrcdevelop.ai.util;

import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.util.Map;

public class SseUtil {

    private SseUtil() {
    }

    /**
     * 发送 ChatBI 文本消息
     *
     * @param emitter SseEmitter
     * @param text    文本消息
     * @throws IOException IO异常
     */
    public static void sendChatBIText(SseEmitter emitter, String text) throws IOException {
        emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder().content(text).type(ChatContentType.TEXT).build(), MediaType.APPLICATION_JSON)
        );
    }

    /**
     * 发送 ChatBI Markdown消息
     *
     * @param emitter SseEmitter
     * @param md    Markdown消息
     * @throws IOException IO异常
     */
    public static void sendChatBIMd(SseEmitter emitter, String md) throws IOException {
        emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder().content(md).type(ChatContentType.MARKDOWN).build(), MediaType.APPLICATION_JSON)
        );
    }

    /**
     * 发送 ChatBI Echarts 图表消息
     *
     * @param emitter SseEmitter
     * @param echarts    Echarts 图表消息
     * @throws IOException IO异常
     */
    public static void sendChatBIEcharts(SseEmitter emitter, Map<String, Object> echarts) throws IOException {
        emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder().content(echarts).type(ChatContentType.ECHARTS).build(), MediaType.APPLICATION_JSON)
        );
    }

    /**
     * 发送 ChatBI 完成消息
     *
     * @param emitter SseEmitter
     * @throws IOException IO异常
     */
    public static void sendChatBIDone(SseEmitter emitter) throws IOException {
        emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder().type(ChatContentType.DONE).build(), MediaType.APPLICATION_JSON)
        );
    }
}
