package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

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
        chatContext.setRelevantTableIds(null);

        Map<String, Object> result = sqlAgent.getRelevantTables(
                chatContext.getChatClient(),
                request.getQueryInstruction(),
                chatContext.getDataSourceId(),
                chatContext.getSampleSqls());
        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(success)) {
            List<String> tables = (List<String>) result.get("tables");
            response.setTables(tables);
            chatContext.setRelevantTableIds(tables);
        }
        response.setSuccess(success);
        response.setError((String) result.get("error"));
        return response;
    }

    @Data
    public static class Request {

        @ToolParam(description = "The query and instruction to get relevant tables")
        private String queryInstruction;
    }

    @Data
    public static class Response {
        private boolean success;

        private List<String> tables;

        private String error;
    }
}
