<#if question??>

Based on the following question, consider the execution of the first step.

### Historical Questions
<#if historical_questions?? && historical_questions?size gt 0>
<#list historical_questions as histQuestion>

- ${histQuestion}
</#list>
</#if>

Question: ${question}

<#else>

Analyze the below tool execution results and the raw user question to determine the next step.

### Historical Questions
<#if historical_questions?? && historical_questions?size gt 0>
The following are the user's previous questions in this conversation for context:
<#list historical_questions as histQuestion>

- ${histQuestion}
</#list>
</#if>

### Multiple SQL Execution Support
You can generate and execute multiple SQL queries in a single conversation. If more data is needed to answer the user's question:
1. Generate a new SQL query based on the current data analysis needs
2. Execute the SQL to get additional data
3. Use the new data along with previous results to form a complete answer

### Decision Criteria
1. **Output Final Answer**: If you have gathered enough data to answer the question, OR if no more useful data can be obtained, output the final answer.
2. **Generate/Execute More SQL**: If you can generate a new SQL query that would provide additional useful data, proceed with generate_sql tool.

### When to Output Final Answer
Output the final answer when ANY of the following is true:
- You have sufficient data to answer the user's question
- You have tried multiple SQL queries but cannot get more useful data
- The data source doesn't contain information to answer the question
- Maximum tool execution attempts have been reached

### Tool Execution Results
<#if tool_execution_results??>

<#list tool_execution_results as item>

- tool: ${item.tool_name}
- execute time: ${item.execute_time}
- result: ${item.result}

</#list>

</#if>

### Raw User Question
${raw_question}

### Decision Output
Based on the above analysis, either:
- Output the final answer (use Final Answer Format) - Prefer this option when data is sufficient or no more useful data can be obtained
- Generate a new SQL query (use Tool Calling Result Format with generate_sql) - Only if you believe another query would provide additional useful data

</#if>

### Mandatory Matters
- The thinking part of the output format cannot contain any Markdown characters('`', '*', '#').
