<#if question??>

Based on the following question, consider the execution of the first step.

Question: ${question}

<#else>

Analyze the below tool execution results and the raw user question to determine the next step.

### Decision Criteria
1. **Sufficient Results**: If all tool execution results are sufficient to answer the question, output the final answer.
2. **Insufficient Results with No Next Step**: If tool execution results are insufficient to answer the question AND there is no logical next tool to execute, output the final answer with clear explanation of why it cannot be answered.
3. **Next Step Available**: If results are insufficient but there is a logical next tool to execute, proceed with the next tool .

### Insufficient Results Assessment
Consider results insufficient when:
- All executed tools returned failure status
- Critical data is missing from successful tool results
- Tool results contradict each other
- Required information for answering is not available in any tool result
- Maximum retry attempts have been exhausted without success

### No Next Step Conditions
Consider no next step available when:
- All relevant tools have been executed and retried
- No additional tools can provide the missing information
- Question requires capabilities beyond available tools
- Data source limitations prevent further analysis

### Final Answer Requirements for Insufficient Results
When outputting final answer due to insufficient results:
- Clearly state that the question cannot be answered with available tools and data
- Specify the specific reasons why it cannot be answered
- List the limitations encountered (e.g., missing data, tool failures, etc.)
- Provide helpful suggestions if possible (e.g., rephrase question, check data availability)

### Tool Execution Results
<#if tool_execution_results??>

<#list tool_execution_results as item>

- tool: ${item.tool_name}
- execute time: ${item.execute_time}
- result: ${item.result}
- 
</#list>

</#if>

### Raw User Question
${raw_question}

### Decision Output
Based on the above analysis, either:
- Output the final answer (use Final Answer Format) if results are sufficient OR insufficient with no next step, 
- Select the next tool to execute (use Tool Calling Result Format) if results are insufficient but next step is available

</#if>

### Mandatory Matters
- The thinking part of the output format cannot contain any Markdown characters('`', '*', '#').