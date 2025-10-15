package cn.opensrcdevelop.ai.chat.tool;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Objects;

@Component
@RequiredArgsConstructor
public class GenerateReportTool implements MethodTool {

    public static final String TOOL_NAME = "generate_report";

    private final AnalyzeAgent analyzeAgent;
    private final AnalyzeDataTool analyzeDataTool;

    @Tool(
            name = TOOL_NAME,
            description = "Generate analysis report for the question"
    )
    public Response generateReport(@ToolParam(description = "The request to generate report") Request request) {
        // 1. 未分析数据时，先执行分析数据工具
        if (Objects.isNull(ChatContext.getAnalyzeDataResult()) || Objects.isNull(ChatContext.getAnalyzeDataSummary())) {
            AnalyzeDataTool.Request analyzeDataRequest = new AnalyzeDataTool.Request();
            analyzeDataRequest.setQuestion(ChatContext.getRawQuestion());
            analyzeDataTool.execute(analyzeDataRequest);
        }

        // 2. 生成分析报告
        Response response = new Response();
        ChatContext.setQuestion(request.getQuestion());
        Map<String, Object> result = analyzeAgent.generateAnalysisReport(
                ChatContext.getChatClient(),
                ChatContext.getAnalyzeDataResult(),
                ChatContext.getAnalyzeDataSummary()
        );

        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(result.get("success"))) {
            ChatContext.setReportType(result.get("report_type").toString());
            ChatContext.setReport(result.get("report").toString());
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

        @ToolParam(description = "The question to generate report")
        private String question;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of generate report")
        private Boolean success;

        @ToolParam(description = "The error message if analyze data failed")
        private String error;
    }
}
