Please verify the legitimacy of the executed SQL query and its results.

### Inputs
1. Executed SQL: ${sql}
2. Sample Query Result(JSON String): ${sample_data}
3. Query Columns:

<#list query_columns as column>

   - ${column.column_name}: ${column.display_name}

</#list>