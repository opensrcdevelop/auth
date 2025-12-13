package cn.opensrcdevelop.ai.util;

import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import io.vavr.control.Try;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.MediaType;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.Random;

public class SseUtil {

    private static final Random RANDOM = new SecureRandom();
    private static final ChatMessageHistoryService chatMessageHistoryService = SpringContextUtil.getBean(ChatMessageHistoryService.class);

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
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(text)
                        .type(ChatContentType.TEXT)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(text, ChatContentType.TEXT);
    }

    /**
     * 随机分段发送文本消息
     *
     * @param emitter  SseEmitter
     * @param text     文本消息
     * @param maxDelay 最大延迟时间（毫秒）
     */
    public static void sendChatBITextSegmented(SseEmitter emitter, String text, ChatContentType contentType, long maxDelay) {
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
                                    .chatId(ChatContextHolder.getChatContext().getChatId())
                                    .questionId(ChatContextHolder.getChatContext().getQuestionId())
                                    .content(segment)
                                    .type(contentType)
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
                                .chatId(ChatContextHolder.getChatContext().getChatId())
                                .questionId(ChatContextHolder.getChatContext().getQuestionId())
                                .content(segment)
                                .type(contentType)
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
        chatMessageHistoryService.createChatMessageHistory(text, contentType);
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
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(md)
                        .type(ChatContentType.MARKDOWN)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(md, ChatContentType.MARKDOWN);
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
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(chart)
                        .type(ChatContentType.CHART)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(CommonUtil.serializeObject(chart), ChatContentType.CHART);
    }

    /**
     * 发送 ChatBI 完成消息
     *
     * @param emitter SseEmitter
     * @param answerId 回答ID
     */
    public static void sendChatBIDone(SseEmitter emitter, String answerId, String rewrittenQuestion) {
        LocalDateTime now = LocalDateTime.now();
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .answerId(answerId)
                        .rewrittenQuestion(rewrittenQuestion)
                        .type(ChatContentType.DONE)
                        .time(now)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(ChatContentType.DONE, answerId, rewrittenQuestion, now);
    }

    /**
     * 发送 ChatBI 完成消息
     *
     * @param emitter SseEmitter
     */
    public static void sendChatBIDone(SseEmitter emitter) {
        LocalDateTime now = LocalDateTime.now();
        Try.run(() -> emitter.send(SseEmitter
                .event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .type(ChatContentType.DONE)
                        .time(now)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(ChatContentType.DONE, null, now);
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
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(loadingMsg)
                        .type(ChatContentType.LOADING)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(loadingMsg, ChatContentType.LOADING);
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
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(table)
                        .type(ChatContentType.TABLE)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(CommonUtil.serializeObject(table), ChatContentType.TABLE);
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
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(errorMsg)
                        .type(ChatContentType.ERROR)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(errorMsg, ChatContentType.ERROR);
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
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(htmlContent)
                        .type(ChatContentType.HTML_REPORT)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        chatMessageHistoryService.createChatMessageHistory(htmlContent, ChatContentType.HTML_REPORT);
    }

    /**
     * 发送 ChatBI 思考消息
     *
     * @param emitter SseEmitter
     * @param thinkingMsg 思考消息
     */
    public static void sendChatBIThinking(SseEmitter emitter, String thinkingMsg, boolean saveMessage) {
        Try.run(() -> emitter.send(SseEmitter.event()
                .data(ChatBIResponseDto.builder()
                        .chatId(ChatContextHolder.getChatContext().getChatId())
                        .questionId(ChatContextHolder.getChatContext().getQuestionId())
                        .content(thinkingMsg)
                        .type(ChatContentType.THINKING)
                        .build(), MediaType.APPLICATION_JSON)
        ));
        if (saveMessage) {
            chatMessageHistoryService.createChatMessageHistory(thinkingMsg, ChatContentType.THINKING);
        }
    }
}
