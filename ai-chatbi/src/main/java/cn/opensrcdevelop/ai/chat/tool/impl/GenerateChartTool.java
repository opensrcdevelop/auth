package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.ChartAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component(GenerateChartTool.TOOL_NAME)
@RequiredArgsConstructor
public class GenerateChartTool implements MethodTool {

    public static final String TOOL_NAME = "generate_chart";

    private final ChartAgent chartAgent;

    @Tool(
            name = TOOL_NAME,
            description = "Used to generate chart based on user question and database query result"
    )
    @SuppressWarnings("unchecked")
    public Response execute(@ToolParam(description = "The request to generate the chart") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        chatContext.setChartConfig(null);
        Response response = new Response();

        Map<String, Object> result = chartAgent.generateChart(
                chatContext.getChatClient(),
                chatContext.getSql(),
                request.question,
                chatContext.getQueryData(),
                request.instruction
        );

        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(success)) {
            Map<String, Object> chartConfig = (Map<String, Object>) result.get("config");

            try {
                ChartRenderer.render(chartConfig, ChatContextHolder.getChatContext().getQueryData());
            } catch (Exception e) {
                response.setSuccess(false);
                response.setError("Failed to render chart: " + e.getMessage() + ", please check the chart config and try again.");
                return response;
            }
            chatContext.setChartConfig(chartConfig);
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

        @ToolParam(description = "The question to generate the chart")
        private String question;

        @ToolParam(description = "The instruction to generate the chart", required = false)
        private String instruction;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of the chart generation")
        private Boolean success;

        @ToolParam(description = "The error message if the chart generation failed")
        private String error;
    }
}
