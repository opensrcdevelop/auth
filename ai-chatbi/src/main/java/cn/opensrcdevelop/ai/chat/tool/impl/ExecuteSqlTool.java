package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component(ExecuteSqlTool.TOOL_NAME)
@RequiredArgsConstructor
@Slf4j
public class ExecuteSqlTool implements MethodTool {

    public static final String TOOL_NAME = "execute_sql";

    private final SqlAgent sqlAgent;
    private final DataSourceManager dataSourceManager;

    @Tool(
            name = TOOL_NAME,
            description = "Used to execute the SQL"
    )
    public Response execute() {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();
        if (StringUtils.isEmpty(chatContext.getSql())) {
            response.setSuccess(false);
            response.setError("No sql found, please execute generate sql tool first.");
            return response;
        }

        chatContext.setQueryData(null);

        var result = executeSqlWithFix(
                chatContext.getChatClient(),
                chatContext.getSql(),
                chatContext.getDataSourceId(),
                chatContext.getRelevantTables(),
                5);
        Boolean success = result._1;
        if (!Boolean.TRUE.equals(success)) {
            response.setError("Failed to execute SQL: %s, error message: %s".formatted(result._3, result._4()));
        } else {
            response.setQueryData(result._2);
            chatContext.setQueryData(result._2);
            chatContext.setSql(result._3);
        }

        response.setSuccess(success);
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of the execute sql")
        private Boolean success;

        @ToolParam(description = "The query data if the execute sql success")
        private List<Map<String, Object>> queryData;

        @ToolParam(description = "The error message if the execute sql failed")
        private String error;
    }

    @SuppressWarnings("all")
    private Tuple4<Boolean, List<Map<String, Object>>, String, String> executeSqlWithFix(ChatClient chatClient,
                                                                                         String sql,
                                                                                         String dataSourceId,
                                                                                         List<Map<String, Object>> relevantTables,
                                                                                         int maxAttempts) {
        JdbcTemplate jdbcTemplate = dataSourceManager.getJdbcTemplate(dataSourceId);
        int attempt = 0;
        List<Map<String, Object>> queryResult = new ArrayList<>();

        while (attempt <= maxAttempts) {
            attempt++;
            try {
                queryResult = jdbcTemplate.queryForList(sql);
                break;
            } catch (Exception ex) {
                String errorMsg = ex.getMessage();
                log.error("第 {} 次执行 SQL 失败", attempt);
                log.error(errorMsg, ex);
                if (attempt > maxAttempts) {
                    return Tuple.of(false, queryResult, sql, errorMsg);
                }
                Map<String, Object> sqlResult = sqlAgent.fixSql(chatClient, sql, errorMsg, relevantTables, dataSourceId);
                if (!Boolean.TRUE.equals(sqlResult.get("success"))) {
                    return Tuple.of(false, queryResult, sql, errorMsg);
                }
                sql = (String) sqlResult.get("sql");
            }
        }

        return Tuple.of(true, queryResult, sql, null);
    }
}
