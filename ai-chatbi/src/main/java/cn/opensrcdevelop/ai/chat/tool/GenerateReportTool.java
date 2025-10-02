package cn.opensrcdevelop.ai.chat.tool;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@RequiredArgsConstructor
public class GenerateReportTool implements MethodTool {

    public static final String TOOL_NAME = "generate_report";

    private final AnalyzeAgent analyzeAgent;

    @Tool(
            name = TOOL_NAME,
            description = "Generate analysis report for the question"
    )
    public Response generateReport(@ToolParam(description = "The request to generate report") Request request) {
        Response response = new Response();

        ChatContext.setQuestion(request.getQuestion());
        Map<String, Object> report = analyzeAgent.generateAnalysisReport(
                ChatContext.getChatClient(),
                ChatContext.getAnalyzeDataResult(),
                ChatContext.getAnalyzeDataSummary()
        );

        if (!Boolean.TRUE.equals(report.get("success"))) {
            response.setSuccess(false);
            response.setError("无法生成分析报告，原因：%s".formatted(report.get("error")));
            return response;
        }

        response.setSuccess(true);
        response.setReport((String) report.get("report"));
        response.setReportType((String) report.get("report_type"));

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

        @ToolParam(description = "The generated report")
        private String report;

        @ToolParam(description = "The type of report, html or markdown")
        private String reportType;

        @ToolParam(description = "The error message if analyze data failed")
        private String error;
    }
}
