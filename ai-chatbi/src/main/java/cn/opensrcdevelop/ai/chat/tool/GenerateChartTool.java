package cn.opensrcdevelop.ai.chat.tool;

import cn.opensrcdevelop.ai.agent.ChartAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
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
        Response response = new Response();

        Map<String, Object> result = chartAgent.generateChart(
                ChatContext.getChatClient(),
                ChatContext.getSql(),
                request.question,
                ChatContext.getQueryData()
        );

        response.setSuccess((Boolean) result.get("success"));
        response.setConfig((Map<String, Object>) result.get("config"));
        response.setError((String) result.get("error"));

        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Override
    public ToolCallback[] getToolCallbacks() {
        return new ToolCallback[0];
    }

    @Data
    public static class Request {

        @ToolParam(description = "The question to generate the chart")
        private String question;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of the chart generation")
        private Boolean success;

        @ToolParam(description = "The configuration of the chart")
        private Map<String, Object> config;

        @ToolParam(description = "The error message if the chart generation failed")
        private String error;
    }
}
