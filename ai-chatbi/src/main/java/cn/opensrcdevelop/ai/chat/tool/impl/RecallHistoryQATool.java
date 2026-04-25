package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.entity.ChatMessageHistory;
import cn.opensrcdevelop.ai.enums.ChatRole;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component(RecallHistoryQATool.TOOL_NAME)
@RequiredArgsConstructor
public class RecallHistoryQATool implements MethodTool {

    public static final String TOOL_NAME = "recall_history_qa";

    private final ChatMessageHistoryService chatMessageHistoryService;

    private final ChatAnswerService chatAnswerService;

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Tool(name = TOOL_NAME, description = "Recall historical question-answer pairs from the current conversation."
            +
            "Use this to retrieve past questions and answers for reference. Only Q&A pairs with successful answers are returned."
            +
            "The index starts from 1 (the first Q&A pair).")
    @SuppressWarnings("java:S3776")
    public Response execute(@ToolParam(description = "The request to recall history QA pairs") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();

        String chatId = chatContext.getChatId();
        if (StringUtils.isEmpty(chatId)) {
            response.setSuccess(false);
            response.setError("No active chat session found.");
            return response;
        }

        List<ChatMessageHistory> userMessages = new ArrayList<>(chatMessageHistoryService.list(
                Wrappers.<ChatMessageHistory>lambdaQuery()
                        .eq(ChatMessageHistory::getChatId, chatId)
                        .eq(ChatMessageHistory::getRole, ChatRole.USER.name())
                        .isNotNull(ChatMessageHistory::getContent)
                        .orderByAsc(ChatMessageHistory::getCreateTime)));

        if (CollectionUtils.isNotEmpty(userMessages)) {
            userMessages.removeLast();
        }

        if (userMessages.isEmpty()) {
            response.setSuccess(true);
            response.setQaPairs(new ArrayList<>());
            response.setTotal(0);
            return response;
        }

        List<String> questionIds = userMessages.stream()
                .map(ChatMessageHistory::getQuestionId)
                .filter(StringUtils::isNotBlank)
                .distinct()
                .collect(Collectors.toList());

        Map<String, ChatAnswer> answerMap = new HashMap<>();
        if (!questionIds.isEmpty()) {
            List<ChatAnswer> answers = chatAnswerService.list(
                    Wrappers.<ChatAnswer>lambdaQuery()
                            .eq(ChatAnswer::getChatId, chatId)
                            .in(ChatAnswer::getQuestionId, questionIds));
            for (ChatAnswer answer : answers) {
                answerMap.put(answer.getQuestionId(), answer);
            }
        }

        List<QAItem> qaPairs = new ArrayList<>();
        for (ChatMessageHistory userMsg : userMessages) {
            String questionId = userMsg.getQuestionId();
            ChatAnswer chatAnswer = answerMap.get(questionId);
            qaPairs.add(new QAItem(userMsg.getContent(), chatAnswer == null ? null : chatAnswer.getAnswer(), chatAnswer));
        }

        if (qaPairs.isEmpty()) {
            response.setSuccess(true);
            response.setQaPairs(new ArrayList<>());
            response.setTotal(0);
            return response;
        }

        int startIndex = request.getStartIndex();
        int endIndex = request.getEndIndex();

        if (startIndex < 1) {
            startIndex = 1;
        }
        if (endIndex > qaPairs.size()) {
            endIndex = qaPairs.size();
        }

        if (startIndex > endIndex) {
            response.setSuccess(true);
            response.setQaPairs(new ArrayList<>());
            response.setTotal(qaPairs.size());
            return response;
        }

        int fromIndex = startIndex - 1;
        int toIndex = endIndex;

        boolean includeSql = Boolean.TRUE.equals(request.getIncludeSql());
        boolean includeReport = Boolean.TRUE.equals(request.getIncludeReport());
        boolean includeChartConfig = Boolean.TRUE.equals(request.getIncludeChartConfig());

        List<Map<String, Object>> resultItems = new ArrayList<>();
        for (int i = fromIndex; i < toIndex; i++) {
            QAItem qaItem = qaPairs.get(i);
            Map<String, Object> item = qaItem.toMap(includeSql, includeReport, includeChartConfig);
            item.put("turn", i + 1);
            resultItems.add(item);
        }

        response.setSuccess(true);
        response.setQaPairs(resultItems);
        response.setTotal(qaPairs.size());
        return response;
    }

    /**
     * 问答对项
     */
    @Data
    @RequiredArgsConstructor
    private static class QAItem {
        private final String question;
        private final String answer;
        private final ChatAnswer chatAnswer;

        public Map<String, Object> toMap(boolean includeSql, boolean includeReport, boolean includeChartConfig) {
            Map<String, Object> map = new HashMap<>();
            map.put("question", question);
            map.put("answer", answer == null ? "This question is not answered." : answer);

            if (includeSql && Objects.nonNull(chatAnswer)) {
                map.put("sql", chatAnswer.getSql());
            }

            if (includeReport && Objects.nonNull(chatAnswer)) {
                map.put("reportType", chatAnswer.getReportType());
                map.put("report", chatAnswer.getReport());
            }

            if (includeChartConfig && Objects.nonNull(chatAnswer)) {
                map.put("chartConfig", chatAnswer.getChartConfig());
            }

            return map;
        }
    }

    @Data
    public static class Request {

        @ToolParam(description = "Start index of the history QA pairs to recall (1-based, e.g., 1 = first Q&A pair). Default is 1.", required = false)
        private int startIndex = 1;

        @ToolParam(description = "End index of the history QA pairs to recall (1-based, e.g., 5 = fifth Q&A pair). Default is 10.", required = false)
        private int endIndex = 10;

        @ToolParam(description = "Whether to include the SQL query in the response. Default is false.", required = false)
        private Boolean includeSql = false;

        @ToolParam(description = "Whether to include the report and report type in the response. Default is false.", required = false)
        private Boolean includeReport = false;

        @ToolParam(description = "Whether to include the chart configuration in the response. Default is false.", required = false)
        private Boolean includeChartConfig = false;
    }

    @Data
    public static class Response {

        @ToolParam(description = "Whether the recall operation was successful")
        private boolean success;

        @ToolParam(description = "Error message if the recall operation failed")
        private String error;

        @ToolParam(description = "Total number of successful QA pairs in the conversation")
        private int total;

        @ToolParam(description = "List of recalled QA pairs, each containing question and answer (and optionally SQL, report, chart config based on request parameters)")
        private List<Map<String, Object>> qaPairs;
    }
}
