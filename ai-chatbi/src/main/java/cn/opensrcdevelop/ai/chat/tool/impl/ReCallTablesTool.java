package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.service.TableService;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component(ReCallTablesTool.TABLE_NAME)
@RequiredArgsConstructor
public class ReCallTablesTool implements MethodTool {

    public static final String TABLE_NAME = "recall_tables";

    private final TableService tableService;

    @Tool(
            name = TABLE_NAME,
            description = "Recall the tables of data source"
    )
    public Response execute() {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();
        String dataSourceId = chatContext.getDataSourceId();
        List<Map<String, Object>> tables = tableService.getTables(dataSourceId);
        if (CollectionUtils.isEmpty(tables)) {
            response.setSuccess(false);
            response.setError("The data source " + dataSourceId + " does not have any available tables.");
        }
        response.setSuccess(true);
        response.setTables(tables);
        return response;
    }

    @Override
    public String toolName() {
        return TABLE_NAME;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of recall data source tables")
        private Boolean success;

        @ToolParam(description = "The error message if recall data source tables failed")
        private String error;

        @ToolParam(description = "The tables of data source")
        private List<Map<String, Object>> tables;
    }
}
