package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component(GenerateSqlTool.TOOL_NAME)
@RequiredArgsConstructor
public class GenerateSqlTool implements MethodTool {

    public static final String TOOL_NAME = "generate_sql";

    private final SqlAgent sqlAgent;

    @SuppressWarnings("unchecked")
    @Tool(
            name = TOOL_NAME,
            description = "Generate SQL from the query"
    )
    public Response execute(@ToolParam(description = "The request to generate SQL") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();
        chatContext.setSql(null);

        String query = request.getQuery();
        List<Map<String, Object>> tables = request.getTables();
        if (StringUtils.isEmpty(query)) {
            query = chatContext.getUserQuery();
        }

        if (CollectionUtils.isEmpty(tables)) {
            tables = chatContext.getRelevantTables();
        }

        Map<String, Object> result = sqlAgent.generateSql(
                chatContext.getChatClient(),
                query,
                tables,
                chatContext.getDataSourceId(),
                request.instruction);
        Boolean success = (Boolean) result.get("success");
        if (Boolean.TRUE.equals(success)) {
            String sql =(String) result.get("sql");
            List<Map<String, Object>> columns = (List<Map<String, Object>>) result.get("columns");
            response.setSql(sql);
            chatContext.setSql(sql);
            chatContext.setQueryColumns(columns);
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

        @ToolParam(description = "The query to generate SQL", required = false)
        private String query;

        @ToolParam(description = "The tables to generate SQL", required = false)
        private List<Map<String, Object>> tables;

        @ToolParam(description = "The instruction to generate SQL", required = false)
        private String instruction;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of generate SQL")
        private Boolean success;

        @ToolParam(description = "The generated SQL if generate SQL success")
        private String sql;

        @ToolParam(description = "The error message if generate SQL failed")
        private String error;
    }
}
