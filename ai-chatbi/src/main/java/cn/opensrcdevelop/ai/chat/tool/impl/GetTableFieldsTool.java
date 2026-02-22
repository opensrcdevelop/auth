package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.service.TableService;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Component(GetTableFieldsTool.TOOL_NAME)
@RequiredArgsConstructor
public class GetTableFieldsTool implements MethodTool {

    public static final String TOOL_NAME = "get_table_fields";

    private final TableService tableService;

    @Tool(name = TOOL_NAME, description = "Get the field definitions of a specific table")
    public Response execute(@ToolParam(description = "The request to get table fields") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();

        String tableId = request.getTableId();
        if (StringUtils.isEmpty(tableId)) {
            response.setSuccess(false);
            response.setError("Table ID is required");
            return response;
        }

        List<Map<String, Object>> fields = tableService.getTableFields(tableId);
        if (CollectionUtils.isEmpty(fields)) {
            response.setSuccess(false);
            response.setError("No fields found for table: " + tableId);
            return response;
        }

        response.setSuccess(true);
        response.setFields(fields);
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {

        @ToolParam(description = "The table ID to get fields for", required = true)
        private String tableId;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of get table fields")
        private boolean success;

        @ToolParam(description = "The field definitions of the table")
        private List<Map<String, Object>> fields;

        @ToolParam(description = "The error message if get table fields failed")
        private String error;
    }
}
