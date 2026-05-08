package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.component.QueryResultTempFileManager;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import jakarta.validation.constraints.Min;
import java.util.List;
import java.util.Map;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Component(ReadQueryResultTool.TOOL_NAME)
@RequiredArgsConstructor
@Slf4j
public class ReadQueryResultTool implements MethodTool {

    public static final String TOOL_NAME = "read_query_result";

    private final QueryResultTempFileManager queryResultTempFileManager;

    @Tool(name = TOOL_NAME, description = "Read query result data from temp file with pagination support. "
            + "Use this tool when the query result is too large and stored in a temp file. "
            + "AI can call this multiple times with different offset and limit to get all data.")
    public Response execute(@ToolParam(description = "The request to read query result") Request request) {
        Response response = new Response();

        String tempFilePath = request.getFilePath();
        if (StringUtils.isEmpty(tempFilePath)) {
            ChatContext chatContext = ChatContextHolder.getChatContext();
            List<String> paths = chatContext.getQueryResultFilePaths();
            tempFilePath = (paths != null && !paths.isEmpty()) ? paths.getLast() : null;
        }

        if (StringUtils.isEmpty(tempFilePath)) {
            response.setSuccess(false);
            response.setError("No temp file found. Please execute SQL first.");
            return response;
        }

        int offset = request.getOffset();
        int limit = request.getLimit();

        if (offset < 0) {
            offset = 0;
        }
        if (limit <= 0) {
            limit = 100;
        }

        List<Map<String, Object>> queryData = queryResultTempFileManager.readLinesFromTempFile(tempFilePath, offset,
                limit);
        if (queryData == null) {
            response.setSuccess(false);
            response.setError("Failed to read temp file or file not found: " + tempFilePath);
            return response;
        }

        response.setSuccess(true);
        response.setQueryData(queryData);
        response.setRecordCount(queryData.size());
        response.setHasMore(queryData.size() == limit);

        log.info("从临时文件读取 {} 条数据，offset={}, limit={}", queryData.size(), offset, limit);
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {

        @ToolParam(description = "The temp file path returned from generate_execute_sql tool response (optional if not provided, will use the latest file from ChatContext)", required = false)
        @NotBlankStr
        private String filePath;

        @ToolParam(description = "The starting offset position (0-based index)")
        @Min(0)
        private int offset;

        @ToolParam(description = "The maximum number of records to read (recommended: 100-500)")
        @Min(0)
        private int limit;
    }

    @Data
    public static class Response {
        private Boolean success;

        private List<Map<String, Object>> queryData;

        private Integer recordCount;

        private Boolean hasMore;

        private String error;
    }
}
