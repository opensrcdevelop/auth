package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Component
@RequiredArgsConstructor
public class SqlAgent {

    private final DataSourceManager dataSourceManager;
    private final TableService tableService;
    private final PromptTemplate promptTemplate;

    @Value("${chatbi.sql-result-limit:1000}")
    private Integer defaultSqlResultLimit;

    /**
     * 从表描述中获取相关表
     *
     * @param chatClient
     *            ChatClient
     * @param userQuestion
     *            用户问题
     * @param dataSourceId
     *            数据源ID
     * @param instruction
     *            指令
     * @param sampleSqls
     *            示例 SQL（问题-SQL 对列表）
     * @return 相关表
     */
    public Map<String, Object> getRelevantTables(ChatClient chatClient,
            String userQuestion,
            String dataSourceId,
            String instruction,
            List<Map<String, String>> sampleSqls) {
        // 1. 获取数据源中的表信息
        List<Map<String, Object>> candidateTables = tableService.getTables(dataSourceId);
        if (CollectionUtils.isEmpty(candidateTables)) {
            return Map.of(
                    "success", false,
                    "error", "The data source " + dataSourceId + " does not have any available tables.");
        }

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.SELECT_TABLE)
                .param("question", userQuestion)
                .param("table_descriptions",
                        CommonUtil.stream(candidateTables).map(CommonUtil::serializeObject).toList())
                .param("instruction", instruction)
                .param("sample_sqls", CollectionUtils.isEmpty(sampleSqls) ? new ArrayList<>() : sampleSqls);

        // 2. 推测关联表
        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.SELECT_TABLE))
                .user(prompt.buildUserPrompt(PromptTemplate.SELECT_TABLE))
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.SELECT_TABLE))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 生成 SQL
     *
     * @param chatClient
     *            ChatClient
     * @param userQuestion
     *            用户问题
     * @param relevantTables
     *            相关表
     * @param dataSourceId
     *            数据源ID
     * @param instruction
     *            指令
     * @param sampleSqls
     *            示例 SQL（问题-SQL 对列表）
     * @return SQL
     */
    public Map<String, Object> generateSql(ChatClient chatClient,
            String userQuestion,
            List<String> relevantTables,
            String dataSourceId,
            String instruction,
            List<Map<String, String>> sampleSqls) {
        // 1. 获取关联表的 Schema
        List<Map<String, Object>> schemas = tableService.getTableSchemas(relevantTables);

        // SQL 结果条数限制
        var chatConfig = ChatContextHolder.getChatContext().getChatConfig();
        int sqlResultLimit = Objects.nonNull(chatConfig) && Objects.nonNull(chatConfig.getSqlResultLimit())
                ? chatConfig.getSqlResultLimit()
                : defaultSqlResultLimit;

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.GENERATE_SQL)
                .param("sql_syntax", dataSourceManager.getDataSourceType(dataSourceId).getDialectName())
                .param("current_time",
                        LocalDateTime.now().format(
                                DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)))
                .param("question", userQuestion)
                .param("relevant_tables", schemas)
                .param("instruction", instruction)
                .param("sample_sqls", CollectionUtils.isEmpty(sampleSqls) ? new ArrayList<>() : sampleSqls)
                .param("sql_result_limit", sqlResultLimit);

        // 2. 生成 SQL
        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.GENERATE_SQL))
                .user(prompt.buildUserPrompt(PromptTemplate.GENERATE_SQL))
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.GENERATE_SQL))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 修复 SQL
     *
     * @param chatClient
     *            ChatClient
     * @param sql
     *            SQL
     * @param error
     *            错误信息
     * @param relevantTables
     *            相关表
     * @param dataSourceId
     *            数据源ID
     * @param instruction
     *            指令
     * @return 修复后的 SQL
     */
    public Map<String, Object> fixSql(ChatClient chatClient,
            String sql,
            String error,
            List<String> relevantTables,
            String dataSourceId,
            String instruction) {
        // 1. 获取关联表的 Schema
        List<Map<String, Object>> schemas = tableService.getTableSchemas(relevantTables);

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.FIX_SQL)
                .param("sql_syntax", dataSourceManager.getDataSourceType(dataSourceId).getDialectName())
                .param("sql", sql)
                .param("error", error)
                .param("relevant_tables", schemas)
                .param("instruction", instruction);

        // 2. 修复 SQL
        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.FIX_SQL))
                .user(prompt.buildUserPrompt(PromptTemplate.FIX_SQL))
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.FIX_SQL))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 检查查询数据是否包含禁止字段
     *
     * @param chatClient
     *            ChatClient
     * @param sql
     *            SQL
     * @param relevantTableIds
     *            相关表ID列表
     * @param queryColumns
     *            查询列
     * @param queryData
     *            查询数据
     * @return 是否包含禁止字段
     */
    public Map<String, Object> checkQueryData(
            ChatClient chatClient,
            String sql,
            List<String> relevantTableIds,
            List<Map<String, Object>> queryColumns,
            List<Map<String, Object>> queryData) {
        // 1. 获取表禁止字段信息
        List<Table> tables = tableService.listByIds(relevantTableIds);
        Map<String, List<String>> forbiddenFieldsMap = tableService.getTableForbiddenFields(relevantTableIds);

        List<Map<String, Object>> relevantTables = new ArrayList<>();
        for (Table table : tables) {
            Map<String, Object> tableInfo = new HashMap<>();
            tableInfo.put("table_name", table.getTableName());
            tableInfo.put("forbidden_fields", forbiddenFieldsMap.get(table.getTableId()));
            relevantTables.add(tableInfo);
        }

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.CHECK_QUERY_DATA)
                .param("relevant_tables", relevantTables)
                .param("sql", sql)
                .param("query_columns", queryColumns)
                .param("sample_data", CommonUtil.nonJdkSerializeObject(queryData.getFirst()));

        // 2. 检查查询数据是否包含禁止字段
        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.CHECK_QUERY_DATA))
                .user(prompt.buildUserPrompt(PromptTemplate.CHECK_QUERY_DATA))
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.CHECK_QUERY_DATA))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }
}
