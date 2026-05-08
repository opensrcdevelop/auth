package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.ChartAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import jakarta.validation.constraints.NotBlank;
import java.util.Map;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Component(GenerateChartTool.TOOL_NAME)
@RequiredArgsConstructor
public class GenerateChartTool implements MethodTool {

    public static final String TOOL_NAME = "generate_chart";

    private final ChartAgent chartAgent;

    @Tool(name = TOOL_NAME, description = "Used to generate chart based on user question and the final SQL query result")
    @SuppressWarnings("unchecked")
    public Response execute(@ToolParam(description = "The request to generate the chart") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        chatContext.setChartConfig(null);
        Response response = new Response();

        if (StringUtils.isEmpty(chatContext.getSql()) || CollectionUtils.isEmpty(chatContext.getQueryData())) {
            response.setSuccess(false);
            response.setError("No SQL Query Result Found, please generate a query first and run it.");
            return response;
        }

        if (!Boolean.TRUE.equals(chatContext.getFinalSqlReviewed())) {
            response.setSuccess(false);
            response.setError("The generated final SQL is not reviewed, please call tool review_sql first.");
            return response;
        }

        if (!Boolean.TRUE.equals(chatContext.getFinalSqlValid())) {
            response.setSuccess(false);
            response.setError(
                    "The generated final SQL is not valid, please call tool generate_execute_sql to regenerate.");
            return response;
        }

        Map<String, Object> result = chartAgent.generateChart(
                chatContext.getChatClient(),
                chatContext.getSql(),
                chatContext.getQueryData(),
                request.instruction);

        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(success)) {
            Map<String, Object> chartConfig = (Map<String, Object>) result.get("config");
            try {
                ChartRenderer.render(chartConfig, ChatContextHolder.getChatContext().getQueryData());
            } catch (Exception e) {
                response.setSuccess(false);
                response.setError(
                        "Failed to render chart: " + e.getMessage() + ", please check the chart config and try again.");
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

        @ToolParam(description = "The instruction to generate the chart")
        @NotBlank
        private String instruction;
    }

    @Data
    public static class Response {
        private Boolean success;

        private String error;
    }
}
