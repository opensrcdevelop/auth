<#if question??>
Based on the following question, consider the execution of the first step.
<#if historical_questions?? && historical_questions?size gt 0>
### Historical Questions
<#list historical_questions as histQuestion>
- ${histQuestion}
</#list>
</#if>

<#if sample_sqls?? && sample_sqls?size gt 0>
### Sample SQL References
The following are similar questions and their SQL queries for reference:
<#list sample_sqls as sample>
**Question:** ${sample.question}
**SQL:** ${sample.sql}
</#list>
</#if>
### Current User Question
${question}

<#else>

Analyze the below tool execution results and the raw user question to determine the next step.
<#if historical_questions?? && historical_questions?size gt 0>
### Historical Questions
The following are the user's previous questions in this conversation for context:
<#list historical_questions as histQuestion>
- ${histQuestion}
</#list>
</#if>

<#if sample_sqls?? && sample_sqls?size gt 0>
### Sample SQL References
The following are similar questions and their SQL queries for reference:
<#list sample_sqls as sample>
**Question:** ${sample.question}
**SQL: ${sample.sql}
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

<#if tool_execution_results?? && tool_execution_results?size gt 0>
### Tool Execution Results
<#list tool_execution_results as item>
<#if item.tool_name??>
- tool: ${item.tool_name}
  - execute time: ${item.execute_time}
  - result: ${item.result}
</#if>
</#list>
</#if>

<#if previous_thinking?? && previous_thinking != "">
### Previous Thinking
Your previous thinking: ${previous_thinking}
</#if>

### Raw User Question
${raw_question}
</#if>

** Decision Output **
Based on the above analysis, either:
- Output the final answer (use Final Answer Format) - When data is sufficient or no more useful data can be obtained
- Call a tool (use Tool Calling Result Format) - When you need more information to provide a final answer

** Mandatory Matters **
<#if show_thinking?? && show_thinking>
- The thinking process must be outputted, and direct output of tool call or final answers is prohibited. Furthermore, after the output of the thinking process is completed, a separator --- must be outputted.
- The thinking process of the output format cannot contain any Markdown characters('`', '*', '#').
- Keep your thinking/reasoning process concise, maximum 300 characters.
</#if>
