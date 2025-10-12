package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class SqlAgent {

    private final DataSourceManager dataSourceManager;
    private final TableService tableService;
    private final PromptTemplate promptTemplate;


    /**
     * 从表描述中获取相关表
     *
     * @param chatClient ChatClient
     * @param userQuestion 用户问题
     * @param dataSourceId 数据源ID
     * @return 相关表
     */
    public Map<String, Object> getRelevantTables(ChatClient chatClient, String userQuestion, String dataSourceId) {
        // 1. 获取数据源中的表信息
        List<Table> candidateTables = tableService.list(Wrappers.<Table>lambdaQuery()
                .eq(Table::getDataSourceId, dataSourceId)
                .eq(Table::getToUse, true));

        if (CollectionUtils.isEmpty(candidateTables)) {
            return Map.of(
                    "success", false,
                    "error", "数据源中没有可用的表"
            );
        }

        List<String> tableDescriptions = candidateTables.stream().map(table -> {
            Map<String, String> tableDescription = new HashMap<>();
            tableDescription.put("table_id", table.getTableId());
            tableDescription.put("table_name", table.getTableName());
            tableDescription.put("description", table.getRemark() == null ? "No description available" : table.getRemark());
            tableDescription.put("additional_info", table.getAdditionalInfo() == null ? "No additional info available" : table.getAdditionalInfo());
            return CommonUtil.serializeObject(tableDescription);
        }).toList();

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.SELECT_TABLE)
                .param("question", userQuestion)
                .param("table_descriptions", tableDescriptions);

        // 2. 推测关联表
        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.SELECT_TABLE))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }

    /**
     * 生成 SQL
     *
     * @param chatClient ChatClient
     * @param userQuestion 用户问题
     * @param relevantTables 相关表
     * @param dataSourceId 数据源ID
     * @return SQL
     */
    public Map<String, Object> generateSql(ChatClient chatClient, String userQuestion, List<Map<String, Object>> relevantTables, String dataSourceId) {
        // 1. 获取关联表的 Schema
        List<Map<String, Object>> schemas = tableService.getTableSchemas(relevantTables);

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.GENERATE_SQL)
                .param("sql_syntax", dataSourceManager.getDataSourceType(dataSourceId).getDialectName())
                .param("current_time", LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)))
                .param("question", userQuestion)
                .param("relevant_tables", schemas);

        // 2. 生成 SQL
        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.GENERATE_SQL))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }

    /**
     * 修复 SQL
     *
     * @param chatClient ChatClient
     * @param sql SQL
     * @param error 错误信息
     * @param relevantTables 相关表
     * @param dataSourceId 数据源ID
     * @return 修复后的 SQL
     */
    public Map<String, Object> fixSql(ChatClient chatClient, String sql, String error, List<Map<String, Object>> relevantTables, String dataSourceId) {
        // 1. 获取关联表的 Schema
        List<Map<String, Object>> schemas = tableService.getTableSchemas(relevantTables);

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.FIX_SQL)
                .param("sql_syntax", dataSourceManager.getDataSourceType(dataSourceId).getDialectName())
                .param("sql", sql)
                .param("error", error)
                .param("relevant_tables", schemas);

        // 2. 修复 SQL
        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.FIX_SQL))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }
}
