package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.service.TableService;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
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
        Response response = new Response();

        List<String> tableIds = request.getTableIds();
        List<Map<String, Object>> fields = tableService.getTableSchemas(tableIds);
        for (Map<String, Object> schema : fields) {
            schema.remove("description");
            schema.remove("additional_info");
        }

        if (CollectionUtils.isEmpty(fields)) {
            response.setSuccess(false);
            response.setError("No fields found for tables: " + tableIds);
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

        @ToolParam(description = "The table ids to get fields for")
        @NotEmpty
        private List<@NotBlank String> tableIds;
    }

    @Data
    public static class Response {
        private boolean success;

        private List<Map<String, Object>> fields;

        private String error;
    }
}
