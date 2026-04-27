### Current Time
${current_time}

### Query And Instruction
${query_instruction}

<#if sample_sqls?? && sample_sqls?size gt 0>
### Sample SQL References
The following are similar questions and their SQL queries for reference:
<#list sample_sqls as sample>
**Question:** ${sample.question}
**SQL:**
```sql
${sample.sql}
```
</#list>
</#if>
