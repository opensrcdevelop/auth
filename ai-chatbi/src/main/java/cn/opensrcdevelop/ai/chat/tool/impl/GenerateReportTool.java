package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import jakarta.validation.constraints.NotBlank;
import java.util.Map;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Component(GenerateReportTool.TOOL_NAME)
@RequiredArgsConstructor
public class GenerateReportTool implements MethodTool {

    public static final String TOOL_NAME = "generate_report";

    private final AnalyzeAgent analyzeAgent;

    @Tool(name = TOOL_NAME, description = "Generate analysis report for the question")
    public Response execute(@ToolParam(description = "The request to generate report") Request request) {
        Response response = new Response();
        ChatContext chatContext = ChatContextHolder.getChatContext();

        // 检查是否存在查询数据
        if (CollectionUtils.isEmpty(ChatContextHolder.getChatContext().getQueryData())) {
            response.setSuccess(false);
            response.setError("The query data is empty, check the sql is executed");
            return response;
        }

        // 检查是否已审核 SQL
        if (!Boolean.TRUE.equals(chatContext.getFinalSqlReviewed())) {
            response.setSuccess(false);
            response.setError("The generated final SQL is not reviewed, please call tool review_sql first.");
            return response;
        }

        // 检查是否已生成有效 SQL
        if (!Boolean.TRUE.equals(chatContext.getFinalSqlValid())) {
            response.setSuccess(false);
            response.setError(
                    "The generated final SQL is not valid, please call tool generate_execute_sql to regenerate.");
            return response;
        }

        Map<String, Object> result = analyzeAgent.generateAnalysisReport(
                chatContext.getChatClient(),
                chatContext.getAnalyzeDataResult(),
                chatContext.getAnalyzeDataSummary(),
                request.getInstruction());

        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(result.get("success"))) {
            chatContext.setReportType(result.get("report_type").toString());
            chatContext.setReport(result.get("report").toString());
        }
        response.setSuccess(success);
        response.setError((String) result.get("error"));

        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {

        @ToolParam(description = "The instruction to generate report")
        @NotBlank
        private String instruction;
    }

    @Data
    public static class Response {
        private Boolean success;

        private String error;
    }
}
