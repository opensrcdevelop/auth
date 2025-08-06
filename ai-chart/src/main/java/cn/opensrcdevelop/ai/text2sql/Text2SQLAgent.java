package cn.opensrcdevelop.ai.text2sql;

import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.entity.Table;
import cn.opensrcdevelop.ai.entity.TableField;
import cn.opensrcdevelop.ai.service.TableFieldService;
import cn.opensrcdevelop.ai.service.TableService;
import cn.opensrcdevelop.ai.util.PromptTemplateUtil;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class Text2SQLAgent {

    private final DataSourceManager dataSourceManager;
    private final TableService tableService;
    private final TableFieldService tableFieldService;

    /**
     * 从表描述中获取相关表
     *
     * @param chatClient ChatClient
     * @param userQuery 用户查询
     * @param dataSourceId 数据源ID
     * @return 相关表
     */
    public List<Map<String, Object>> getRelevantTables(ChatClient chatClient, String userQuery, String dataSourceId) {
        // 获取数据源关联的表信息
        List<Table> candidateTables = tableService.list(Wrappers.<Table>lambdaQuery()
                .eq(Table::getDataSourceId, dataSourceId)
                .eq(Table::getToUse, true));

        if (CollectionUtils.isEmpty(candidateTables)) {
            return Collections.emptyList();
        }

        List<String> tableDescriptions = candidateTables.stream().map(table -> {
            Map<String, String> tableDescription = new HashMap<>();
            tableDescription.put("table_id", table.getTableId());
            tableDescription.put("table_name", table.getTableName());
            tableDescription.put("description", table.getRemark() == null ? "No description available" : table.getRemark());
            tableDescription.put("additional_info", table.getAdditionalInfo() == null ? "No additional info available" : table.getAdditionalInfo());
            return CommonUtil.serializeObject(tableDescription);
        }).toList();

        String userText =
                """
                   Given the user's query: '${user_query}', and the following table descriptions:
                   <#list table_descriptions as item>
                   - ${item}
                   </#list>
                   Please identify which tables are relevant to answer the query, including both fact tables and dimension tables.
                   Then, return a JSON array containing objects with the original JSON information of the aforementioned optional table
                   Please ensure that the 'table' names are in lower case.
                   For example: [{"table_id": "table_id", "table_name": "table_name", "description": "description", "additional_info": "additional_info"}, ...]
                """;
        return chatClient.prompt()
                .system("You are an expert in database schema understanding.")
                .user(PromptTemplateUtil.getPrompt(userText, Map.of(
                        "user_query", userQuery,
                        "table_descriptions", tableDescriptions
                )))
                .call()
                .entity(new ParameterizedTypeReference<List<Map<String, Object>>>() {});
    }

    /**
     * 生成 SQL
     *
     * @param chatClient ChatClient
     * @param userQuery 用户查询
     * @param relevantTables 相关表
     * @param dataSourceId 数据源ID
     * @return SQL
     */
    public String generateSql(ChatClient chatClient, String userQuery, List<Map<String, Object>> relevantTables, String dataSourceId) {
        // 获取表关联的字段信息
        List<String> tableIds = CommonUtil.stream(relevantTables).map(x -> x.get("table_id").toString()).toList();
        List<TableField> allTableFields = tableFieldService.list(Wrappers.<TableField>lambdaQuery()
                .in(TableField::getTableId, tableIds));

        List<Map<String, Object>> newRelevantTables = CommonUtil.stream(relevantTables).map(table -> {
            Map<String, Object> newTableInfo = new HashMap<>(table);
            List<TableField> tableFields = CommonUtil.stream(allTableFields).filter(x -> x.getTableId().equals(table.get("table_id"))).toList();
            List<String> fieldDescriptions = CommonUtil.stream(tableFields).map(x -> {
                Map<String, String> fieldDescription = new HashMap<>();
                fieldDescription.put("field_name", x.getFieldName());
                fieldDescription.put("field_data_type", x.getFieldType());
                fieldDescription.put("description", x.getRemark() == null ? "No description available":  x.getRemark());
                fieldDescription.put("additional_info", x.getAdditionalInfo() == null ? "No additional info available" : x.getAdditionalInfo());
                return CommonUtil.serializeObject(fieldDescription);
            }).toList();
            newTableInfo.put("fields", fieldDescriptions);
            return newTableInfo;
        }).toList();

        String currentDate = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd"));
        String userText =
                """
                You are an expert in SQL query generation.
                Your task is to generate an accurate SQL query based strictly on the given information.
                
                ### Current date:
                ${current_date}
                
                ### User's Question:
                ${user_query}
                
                ### Relevant Tables:
                <#list relevant_tables as table>
                - **Table**: ${table.table_name}
                  - **Description**: ${table.description}
                  - **Additional Info**: ${table.additional_info}
                  - **Columns**:
                  <#list table.fields as item>
                    - ${item}
                  </#list>
                </#list>
                
                ### Reasoning Process:
                1. **Identify the relevant tables**: Determine which tables are needed based on the user question.
                2. **Extract required fields**: Identify the columns that should be included in the query.
                3. **Construct the SQL query**: Assemble the final query using the extracted information, ensuring it follows SQL syntax and best practices.
                4. **Avoid assumptions**: Do not assume any table join relationships that are not explicitly provided in the information.
                5. **Finalize the SQL query**: Provide a well-structured and clear SQL query that answers the user's question accurately.
                
                ### Output Format:
                Provide only the SQL query in your response. No explanations or additional text.
                **Strict Constraints:**
                - Use only the provided tables, columns, and code table values.
                - Do not use any table, column, or value not explicitly listed.
                - If you cannot generate a valid SQL query, return an empty string.
                """;
        return chatClient.prompt()
                .system("You are an expert in generating SQL queries from natural language, specifically for %s syntax.".formatted(dataSourceManager.getDataSourceType(dataSourceId).getDialectName()))
                .user(PromptTemplateUtil.getPrompt(userText, Map.of(
                        "current_date", currentDate,
                        "user_query", userQuery,
                        "relevant_tables", newRelevantTables
                )))
                .call()
                .content();
    }

    public Map<String, Object> generateEchartsConfig(ChatClient chatClient, String userQuery, String sql, String queryResult) {

        String userText =
                """
                Your task is to produce a valid ECharts option object (JSON) based strictly on the given information.
               
                ### User's Question:
                ${user_query}
               
                ### Executed SQL:
                ${sql}
               
                ### Query Result:
                ${query_result}
               
                ### Reasoning Process:
                1. **Understand the data**: Analyze the query result structure and semantics.
                2. **Determine the chart type**: Choose the most appropriate ECharts chart type (bar, line, pie, scatter, etc.) that best answers the user's question.
                3. **Map fields to dimensions**: Identify which fields should be used as x-axis, y-axis, series, or other dimensions.
                4. **Transform the data**: Convert the query result into the exact format required by the chosen ECharts chart type.
                5. **Compose the option object**: Construct a complete, valid ECharts option object including title, legend, tooltip, axes, series, and any necessary styling.
                6. **Avoid assumptions**: Do not add any data, fields, or values that are not explicitly present in the query result.
               
                ### Output Format:
                Return **only** the JSON string of the ECharts option object.
                Do not include explanations, comments, or any additional text.
                **Strict Constraints:**
                - If you cannot generate a valid ECharts option object (JSON), return an empty string.
                """;
        return chatClient.prompt()
                .system("You are an expert in data visualization and Apache ECharts configuration generation.")
                .user(PromptTemplateUtil.getPrompt(userText, Map.of(
                        "user_query", userQuery,
                        "sql", sql,
                        "query_result", queryResult
                )))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }
}

