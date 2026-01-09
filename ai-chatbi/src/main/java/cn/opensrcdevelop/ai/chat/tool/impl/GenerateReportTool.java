package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Objects;

@Component(GenerateReportTool.TOOL_NAME)
@RequiredArgsConstructor
public class GenerateReportTool implements MethodTool {

    public static final String TOOL_NAME = "generate_report";

    private final AnalyzeAgent analyzeAgent;
    private final AnalyzeDataTool analyzeDataTool;

    @Tool(
            name = TOOL_NAME,
            description = "Generate analysis report for the question"
    )
    public Response execute(@ToolParam(description = "The request to generate report") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        // 1. 未分析数据时，先执行分析数据工具
        if (Objects.isNull(chatContext.getAnalyzeDataResult()) ||
                Objects.isNull(chatContext.getAnalyzeDataSummary())) {
            AnalyzeDataTool.Request analyzeDataRequest = new AnalyzeDataTool.Request();
            analyzeDataRequest.setQuestion(chatContext.getRawQuestion());
            analyzeDataTool.execute(analyzeDataRequest);
        }

        // 2. 生成分析报告
        Response response = new Response();
        chatContext.setQuestion(request.getQuestion());
        Map<String, Object> result = analyzeAgent.generateAnalysisReport(
                chatContext.getChatClient(),
                chatContext.getAnalyzeDataResult(),
                chatContext.getAnalyzeDataSummary(),
                request.instruction
        );

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

        @ToolParam(description = "The question to generate report")
        private String question;

        @ToolParam(description = "The instruction to generate report", required = false)
        private String instruction;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of generate report")
        private Boolean success;

        @ToolParam(description = "The error message if analyze data failed")
        private String error;
    }
}
