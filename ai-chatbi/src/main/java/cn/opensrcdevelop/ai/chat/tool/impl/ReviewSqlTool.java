package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;

@Slf4j
@Component(ReviewSqlTool.TOOL_NAME)
@RequiredArgsConstructor
public class ReviewSqlTool implements MethodTool {

    public static final String TOOL_NAME = "review_sql";

    private final SqlAgent sqlAgent;

    @Tool(name = TOOL_NAME, description = "Review the SQL query to verify if it can answer the user's question. "
            + "This tool must be called before final_answer or before generate_chart, generate_report, analyze_data tools.")
    @SuppressWarnings("all")
    public Response execute(@ToolParam(description = "The review request containing the viewpoint") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();

        String sql = chatContext.getSql();
        String question = chatContext.getQuestion();
        var queryData = chatContext.getQueryData();
        var queryColumns = chatContext.getQueryColumns();

        if (StringUtils.isBlank(sql)) {
            response.setSuccess(false);
            response.setError("SQL is not available in the context. Please generate and execute SQL first.");
            return response;
        }

        if (StringUtils.isBlank(question)) {
            response.setSuccess(false);
            response.setError("Question is not available in the context.");
            return response;
        }

        Map<String, Object> reviewResult = sqlAgent.reviewSql(
                chatContext.getChatClient(),
                sql,
                question,
                queryColumns,
                queryData,
                request.getViewpoint());

        chatContext.setFinalSqlReviewed(true);
        if (Boolean.TRUE.equals(reviewResult.get("success"))) {
            response.setSuccess(true);

            String message = (String) reviewResult.get("message");
            Boolean valid = (Boolean) reviewResult.get("valid");

            response.setValid((Boolean) reviewResult.get("valid"));
            response.setMessage(valid ? message : message + "\n Re-execute Tool generate_execute_sql to ensure that the final generated SQL meets the user's question");
            chatContext.setFinalSqlValid(valid);
        } else {
            response.setSuccess(false);
            response.setError((String) reviewResult.get("error"));
        }

        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {
        @ToolParam(description = "The viewpoint or perspective for reviewing the SQL. "
                + "For example: 'Check if the aggregation logic matches the question', "
                + "'Verify if the date range is correct', etc.")
        @NotBlank
        private String viewpoint;
    }

    @Data
    public static class Response {
        private Boolean success;

        private Boolean valid;

        private String message;

        private String error;
    }
}
