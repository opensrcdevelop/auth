package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Component(GetRelevantTablesTool.TOOL_NAME)
@RequiredArgsConstructor
public class GetRelevantTablesTool implements MethodTool {

    public static final String TOOL_NAME = "get_relevant_tables";

    private final SqlAgent sqlAgent;

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Tool(name = TOOL_NAME, description = "Get the relevant tables for the question")
    @SuppressWarnings("unchecked")
    public Response execute(@ToolParam(description = "The request to get relevant tables") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();
        chatContext.setRelevantTables(null);

        String query = request.getQuery();
        if (StringUtils.isEmpty(request.getQuery())) {
            query = chatContext.getUserQuery();
        }

        Map<String, Object> result = sqlAgent.getRelevantTables(
                chatContext.getChatClient(),
                query,
                chatContext.getDataSourceId(),
                request.instruction,
                chatContext.getSampleSqls());
        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(success)) {
            List<Map<String, Object>> tables = (List<Map<String, Object>>) result.get("tables");
            response.setTables(tables);
            chatContext.setRelevantTables(tables);
        }
        response.setSuccess(success);
        response.setError((String) result.get("error"));
        return response;
    }

    @Data
    public static class Request {

        @ToolParam(description = "The query to get relevant tables", required = false)
        private String query;

        @ToolParam(description = "The instruction to get relevant tables", required = false)
        private String instruction;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of get relevant tables")
        private boolean success;

        @ToolParam(description = "The relevant tables of the query")
        private List<Map<String, Object>> tables;

        @ToolParam(description = "The error message of get relevant tables")
        private String error;
    }
}
