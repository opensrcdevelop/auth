### Current Question
${question}

### Historical Answers
<#list historical_answers as answer>

- answerId: ${answer.answerId}
  question: ${answer.question}
</#list>

### Instruction
Please identify which historical questions are semantically related to the current question (at most ${max_samples} answers).
