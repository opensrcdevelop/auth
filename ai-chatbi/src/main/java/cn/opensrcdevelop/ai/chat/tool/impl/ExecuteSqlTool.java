package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.component.QueryResultTempFileManager;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.util.SseUtil;
import io.vavr.Tuple;
import io.vavr.Tuple4;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Component(ExecuteSqlTool.TOOL_NAME)
@RequiredArgsConstructor
@Slf4j
public class ExecuteSqlTool implements MethodTool {

    public static final String TOOL_NAME = "execute_sql";

    private final SqlAgent sqlAgent;
    private final DataSourceManager dataSourceManager;
    private final QueryResultTempFileManager queryResultTempFileManager;

    @Value("${chatbi.max-sql-execution-retry-count:3}")
    private Integer defaultMaxSqlExecutionRetryCount;

    @Tool(name = TOOL_NAME, description = "Used to execute the SQL")
    public Response execute(@ToolParam(description = "The request to execute SQL") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        SseEmitter emitter = chatContext.getEmitter();
        Response response = new Response();
        if (StringUtils.isEmpty(chatContext.getSql())) {
            response.setSuccess(false);
            response.setError("No sql found, please execute generate sql tool first.");
            return response;
        }

        if (!isLegalSql(chatContext.getSql())) {
            response.setSuccess(false);
            response.setError("Cannot execute the SQL. Please check if it is a query or contains 'SELECT *'.");
            return response;
        }

        chatContext.setQueryData(null);

        var chatConfig = chatContext.getChatConfig();
        int maxSqlExecutionRetryCount = Objects.nonNull(chatConfig)
                && Objects.nonNull(chatConfig.getMaxSqlExecutionRetryCount())
                        ? chatConfig.getMaxSqlExecutionRetryCount()
                        : defaultMaxSqlExecutionRetryCount;

        var result = executeSqlWithFix(
                chatContext.getChatClient(),
                chatContext.getSql(),
                chatContext.getDataSourceId(),
                chatContext.getRelevantTableIds(),
                maxSqlExecutionRetryCount,
                request.fixSqlInstruction,
                emitter);
        Boolean success = result._1;
        if (!Boolean.TRUE.equals(success)) {
            response.setError("Failed to execute SQL: %s, error message: %s".formatted(result._3, result._4()));
        } else {
            List<Map<String, Object>> queryData = result._2;
            chatContext.setQueryData(queryData);
            chatContext.setSql(result._3);

            // 检查数据条数是否超过阈值，超过则写入临时文件
            String tempFilePath = queryResultTempFileManager.writeQueryDataToTempFile(queryData,
                    chatContext.getChatId());
            if (tempFilePath != null) {
                // 超过阈值，数据写入临时文件
                chatContext.addQueryResultFilePath(tempFilePath);
                response.setTempFilePath(tempFilePath);
                response.setRecordCount(queryData.size());
                response.setQueryData(null);
                log.info("查询结果 {} 条已写入临时文件: {}", queryData.size(), tempFilePath);
            } else {
                // 未超过阈值，直接返回数据
                response.setQueryData(queryData);
            }
        }

        response.setSuccess(success);
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {

        @ToolParam(description = "The instruction to fix the SQL, which is used to fix the SQL if it is has syntax error. "
                +
                "If the SQL is legal but execution failed, do not pass the fix instruction.", required = false)
        private String fixSqlInstruction;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of the execute sql")
        private Boolean success;

        @ToolParam(description = "The query data if the execute sql success")
        private List<Map<String, Object>> queryData;

        @ToolParam(description = "The error message if the execute sql failed")
        private String error;

        @ToolParam(description = "The temp file path if the query result exceeds threshold")
        private String tempFilePath;

        @ToolParam(description = "The total record count if the query result exceeds threshold")
        private Integer recordCount;
    }

    @SuppressWarnings("all")
    private Tuple4<Boolean, List<Map<String, Object>>, String, String> executeSqlWithFix(ChatClient chatClient,
            String sql,
            String dataSourceId,
            List<String> relevantTables,
            int maxAttempts,
            String instruction,
            SseEmitter emitter) {
        JdbcTemplate jdbcTemplate = dataSourceManager.getJdbcTemplate(dataSourceId);
        int attempt = 0;
        List<Map<String, Object>> queryResult = new ArrayList<>();

        while (attempt <= maxAttempts) {
            attempt++;
            try {
                // 1. 执行 SQL
                queryResult = jdbcTemplate.queryForList(sql);

                // 2.检查查询数据是否包含禁止字段
                if (CollectionUtils.isNotEmpty(queryResult)) {
                    Map<String, Object> checkResult = sqlAgent.checkQueryData(
                            chatClient,
                            sql,
                            relevantTables,
                            ChatContextHolder.getChatContext().getQueryColumns(),
                            queryResult);
                    Boolean checkSuccess = (Boolean) checkResult.get("success");
                    if (!Boolean.TRUE.equals(checkSuccess)) {
                        return Tuple.of(false, queryResult, sql, (String) checkResult.get("error"));
                    }

                    Boolean valid = (Boolean) checkResult.get("valid");
                    if (!Boolean.TRUE.equals(valid)) {
                        return Tuple.of(false, queryResult, sql, (String) checkResult.get("message"));
                    }
                }

                break;
            } catch (Exception ex) {
                String errorMsg = ex.getMessage();
                log.error("第 {} 次执行 SQL 失败", attempt);
                log.error(errorMsg, ex);
                if (attempt > maxAttempts) {
                    return Tuple.of(false, queryResult, sql, errorMsg);
                }

                try {
                    SseUtil.sendChatBIToolCall(emitter, "第 %d 次执行 SQL 失败，开始修复 SQL".formatted(attempt));
                    Map<String, Object> sqlResult = sqlAgent.fixSql(chatClient, sql, errorMsg, relevantTables,
                            dataSourceId,
                            instruction);
                    if (!Boolean.TRUE.equals(sqlResult.get("success"))) {
                        return Tuple.of(false, queryResult, sql, errorMsg);
                    }
                    sql = (String) sqlResult.get("sql");
                    SseUtil.sendChatBIToolCall(emitter, "修复 SQL 成功，继续执行");
                } catch (Exception newEx) {
                    log.error("修复 SQL 失败", newEx);
                    SseUtil.sendChatBIToolCall(emitter, "修复 SQL 失败");
                    return Tuple.of(false, queryResult, sql, errorMsg);
                }
            }
        }

        return Tuple.of(true, queryResult, sql, null);
    }

    private boolean isLegalSql(String sql) {
        String lowerSql = sql.toLowerCase().trim();

        String[] illegalKeywords = {
                "insert", "update", "delete", "drop", "truncate",
                "alter", "create", "replace", "merge", "execute",
                "exec", "call", "grant", "revoke", "commit", "rollback"
        };

        for (String keyword : illegalKeywords) {
            if (lowerSql.matches("(?i)\\b" + keyword + "\\b.*")) {
                return false;
            }
        }

        if (lowerSql.contains("select *")) {
            return false;
        }

        return lowerSql.startsWith("select") || lowerSql.startsWith("with");
    }
}
