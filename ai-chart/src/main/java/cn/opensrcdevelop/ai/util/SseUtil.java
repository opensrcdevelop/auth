package cn.opensrcdevelop.ai.util;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;

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
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(text)
                        .type(ChatContentType.TEXT)
                        .build(), MediaType.APPLICATION_JSON)
        );
    }

    /**
     * 发送 ChatBI Markdown消息
     *
     * @param emitter SseEmitter
     * @param md      Markdown消息
     * @throws IOException IO异常
     */
    public static void sendChatBIMd(SseEmitter emitter, String md) throws IOException {
        emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(md)
                        .type(ChatContentType.MARKDOWN)
                        .build(), MediaType.APPLICATION_JSON)
        );
    }

    /**
     * 发送 ChatBI 图表
     *
     * @param emitter SseEmitter
     * @param chart   图表
     * @throws IOException IO异常
     */
    public static void sendChatBIChart(SseEmitter emitter, Object chart) throws IOException {
        emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(chart)
                        .type(ChatContentType.ECHARTS)
                        .build(), MediaType.APPLICATION_JSON)
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
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .type(ChatContentType.DONE)
                        .build(), MediaType.APPLICATION_JSON)
        );
    }

    /**
     * 发送 ChatBI Loading
     *
     * @param emitter SseEmitter
     * @param loadingMsg 加载消息
     * @throws IOException IO异常
     */
    public static void sendChatBILoading(SseEmitter emitter, String loadingMsg) throws IOException {
        emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(loadingMsg)
                        .type(ChatContentType.LOADING)
                        .build(), MediaType.APPLICATION_JSON)
        );
    }
}
