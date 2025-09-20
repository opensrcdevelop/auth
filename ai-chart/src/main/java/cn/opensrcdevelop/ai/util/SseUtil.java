package cn.opensrcdevelop.ai.util;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import io.vavr.control.Try;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.security.SecureRandom;
import java.util.Random;

public class SseUtil {

    private static final Random RANDOM = new SecureRandom();

    private SseUtil() {
    }

    /**
     * 发送 ChatBI 文本消息
     *
     * @param emitter SseEmitter
     * @param text    文本消息
     */
    public static void sendChatBIText(SseEmitter emitter, String text) {
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(text)
                        .type(ChatContentType.TEXT)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 随机分段发送文本消息
     *
     * @param emitter  SseEmitter
     * @param text     文本消息
     * @param maxDelay 最大延迟时间（毫秒）
     */
    public static void sendChatBITextSegmented(SseEmitter emitter, String text, long maxDelay) {
        Try.run(() -> {
            if (StringUtils.isEmpty(text)) {
                return;
            }

            // 1. 计算随机分段数量，至少 1 段
            int textLength = text.length();
            int segmentCount = Math.max(1, RANDOM.nextInt(10) + 1);

            // 2. 分段发送文本
            int startIndex = 0;
            for (int i = 0; i < segmentCount; i++) {
                int remainingLength = textLength - startIndex;

                // 2.1 最后一段，直接发送剩余全部文本
                if (i == segmentCount - 1) {
                    String segment = text.substring(startIndex);
                    emitter.send(SseEmitter
                            .event()
                            .data(ChatBIResponseDto.builder()
                                    .chatId(ChatContext.getChatId())
                                    .questionId(ChatContext.getQuestionId())
                                    .content(segment)
                                    .type(ChatContentType.TEXT)
                                    .build(), MediaType.APPLICATION_JSON)
                    );
                    break;
                }

                // 2.2 计算当前段长度，至少 1 个字符，最多不超过剩余长度的一半 + 1，确保能分完
                int maxSegmentLength = Math.max(1, remainingLength / 2);
                int segmentLength = Math.max(1, RANDOM.nextInt(maxSegmentLength) + 1);

                // 2.3 获取并发送分段文本
                int endIndex = Math.min(startIndex + segmentLength, textLength);
                String segment = text.substring(startIndex, endIndex);
                emitter.send(SseEmitter
                        .event()
                        .data(ChatBIResponseDto.builder()
                                .chatId(ChatContext.getChatId())
                                .questionId(ChatContext.getQuestionId())
                                .content(segment)
                                .type(ChatContentType.TEXT)
                                .build(), MediaType.APPLICATION_JSON)
                );
                startIndex = endIndex;

                // 2.4 添加随机延迟
                try {
                    Thread.sleep(RANDOM.nextLong(maxDelay));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        });
    }

    /**
     * 发送 ChatBI Markdown消息
     *
     * @param emitter SseEmitter
     * @param md      Markdown消息
     */
    public static void sendChatBIMd(SseEmitter emitter, String md) {
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(md)
                        .type(ChatContentType.MARKDOWN)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 发送 ChatBI 图表
     *
     * @param emitter SseEmitter
     * @param chart   图表
     */
    public static void sendChatBIChart(SseEmitter emitter, Object chart) {
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(chart)
                        .type(ChatContentType.CHART)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 发送 ChatBI 完成消息
     *
     * @param emitter SseEmitter
     * @param chartId 图表ID
     */
    public static void sendChatBIDone(SseEmitter emitter, String chartId) {
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .actionType(ChatContext.getActionType())
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .chartId(chartId)
                        .type(ChatContentType.DONE)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 发送 ChatBI 完成消息
     *
     * @param emitter SseEmitter
     */
    public static void sendChatBIDone(SseEmitter emitter) {
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .actionType(ChatContext.getActionType())
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .type(ChatContentType.DONE)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 发送 ChatBI Loading
     *
     * @param emitter    SseEmitter
     * @param loadingMsg 加载消息
     */
    public static void sendChatBILoading(SseEmitter emitter, String loadingMsg) {
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(loadingMsg)
                        .type(ChatContentType.LOADING)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 发送 ChatBI 表格
     *
     * @param emitter SseEmitter
     * @param table   表格
     */
    public static void sendChatBITable(SseEmitter emitter, Object table) {
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(table)
                        .type(ChatContentType.TABLE)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 发送 ChatBI 错误消息
     *
     * @param emitter  SseEmitter
     * @param errorMsg 错误消息
     */
    public static void sendChatBIError(SseEmitter emitter, String errorMsg) {
        Try.run(() -> emitter.send(SseEmitter.event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(errorMsg)
                        .type(ChatContentType.ERROR)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }

    /**
     * 发送 ChatBI HTML 报告
     *
     * @param emitter  SseEmitter
     * @param htmlContent HTML 报告内容
     */
    public static void sendChatBIHtmlReport(SseEmitter emitter, String htmlContent) {
        Try.run(() -> emitter.send(SseEmitter.event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContext.getChatId())
                        .questionId(ChatContext.getQuestionId())
                        .content(htmlContent)
                        .type(ChatContentType.HTML_REPORT)
                        .build(), MediaType.APPLICATION_JSON)
        ));
    }
}
