<#if question??>
Based on the current user question, consider the execution of the first step.
<#if historical_questions?? && historical_questions?size gt 0>
### Historical Questions(Sort in ascending order of time)
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
${raw_question}

<#else>

Analyze the below tool execution results and the previous user question to determine the next step.
<#if historical_questions?? && historical_questions?size gt 0>
### Historical Questions(Sort in ascending order of time)
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

### Previous User Question
${raw_question}
</#if>

<#if format_error_feedback?? && format_error_feedback != "">
### ⚠️ Format Error Feedback (IMPORTANT)
The previous output did not meet the required format. Please fix the following issues:
${format_error_feedback}

Please ensure your next output strictly follows the required format.
</#if>

<#if consecutive_failure_warning?? && consecutive_failure_warning != "">
## ⚠️ Consecutive Tool Failure Warning (IMPORTANT)
${consecutive_failure_warning}
</#if>

** Current Time **
${current_time}

** Decision Output **
Based on the above analysis, either:
- Output the final answer (use Final Answer Format) - When there are no tools available to call upon and the information you currently have is sufficient to answer the user's question
- Call a tool (use Tool Calling Result Format) - When you need more information to answer a user's question

** Mandatory Matters **
<#if show_thinking?? && show_thinking>
- The thinking process must be outputted, and direct output of tool call or final answers is prohibited. Furthermore, after the output of the thinking process is completed, a separator --- must be outputted.
- The thinking process of the output format cannot contain any Markdown characters('`', '*', '#').
- Keep your thinking/reasoning process concise, maximum 300 characters.
</#if>
