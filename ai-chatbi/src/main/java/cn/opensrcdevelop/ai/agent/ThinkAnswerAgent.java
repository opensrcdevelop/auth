package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.chat.tool.impl.AnalyzeDataTool;
import cn.opensrcdevelop.ai.chat.tool.impl.AskUserTool;
import cn.opensrcdevelop.ai.chat.tool.impl.ExecutePythonTool;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ValidationException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.fasterxml.jackson.core.JacksonException;
import com.fasterxml.jackson.core.type.TypeReference;
import jakarta.validation.ConstraintViolation;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.ai.chat.model.Generation;
import org.springframework.ai.chat.prompt.Prompt;
import org.springframework.ai.model.tool.ToolCallingChatOptions;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
@Slf4j
public class ThinkAnswerAgent {

    private final PromptTemplate promptTemplate;
    private final List<MethodTool> methodTools;
    private final ChatMessageHistoryService chatMessageHistoryService;

    /**
     * 思考并回答用户提问
     *
     * @param emitter
     *            SSE
     * @param interruptFlag
     *            中断标志
     * @param chatClient
     *            ChatClient
     * @param userQuestion
     *            用户提问
     * @param sampleSqls
     *            示例 SQL（问题-SQL 对）
     * @param historicalQuestions
     *            历史问题
     * @param maxSteps
     *            最大执行步数
     * @param showThinking
     *            是否显示思考过程
     */
    @SuppressWarnings({ "java:S3776", "java:S107" })
    public Map<String, Object> thinkAnswer(SseEmitter emitter,
            AtomicBoolean interruptFlag,
            ChatClient chatClient,
            String userQuestion,
            List<Map<String, String>> sampleSqls,
            List<String> historicalQuestions,
            int maxSteps,
            boolean showThinking) {
        // 将示例 SQL 和历史问题列表存储到上下文
        ChatContextHolder.getChatContext().setSampleSqls(sampleSqls);
        ChatContextHolder.getChatContext().setHistoricalQuestions(historicalQuestions);

        String formatErrorFeedback = null;
        int step = 0;
        while (step < maxSteps) {
            if (interruptFlag.get()) {
                log.info("ChatBI 对话（{}）被中断", ChatContextHolder.getChatContext().getChatId());
                break;
            }

            SseUtil.sendChatBILoading(emitter, "思考中...");
            String stepThinkingMsg = step > 0
                    ? "\n<strong>Step " + (step + 1) + "</strong>\n"
                    : "<strong>Step " + (step + 1) + "</strong>\n";
            SseUtil.sendChatBIThinking(emitter, stepThinkingMsg, true);

            String result = callLlm(emitter, interruptFlag, chatClient, step > 0 ? null : userQuestion, showThinking,
                    formatErrorFeedback);
            if (StringUtils.isEmpty(result)) {
                break;
            }

            // 验证输出格式
            formatErrorFeedback = null;
            var validationResult = validateOutputFormat(result, showThinking);
            if (!validationResult.isValid()) {
                log.warn("Output format validation failed: {}, the llm result is: {}",
                        validationResult.getErrorMessage(), result);
                SseUtil.sendChatBIThinking(emitter, "⚠ The output format of the LLM has encountered an error.", true);

                // 格式验证失败，将错误反馈给下一轮
                formatErrorFeedback = validationResult.getErrorMessage() + "\n Your output is: \n" + result;
                step++;
                continue;
            }

            // 验证通过，使用验证结果中的 JSON 数据
            Map<String, Object> jsonMap = validationResult.getJsonMap();
            String thinkingContent = "";

            if (showThinking) {
                // 提取思考内容
                int startIndex = result.indexOf("{");
                thinkingContent = result.substring(0, startIndex).trim();
                if (thinkingContent.contains("---")) {
                    thinkingContent = thinkingContent.replace("---", "").trim();
                }
            }

            boolean isFinalAnswer = jsonMap.containsKey("final_answer");

            if (StringUtils.isNotEmpty(thinkingContent)) {
                chatMessageHistoryService.createChatMessageHistory(thinkingContent, ChatContentType.THINKING);
            }

            // 保存思考内容到上下文，供下一轮使用
            saveThinkingContent(thinkingContent);
            if (isFinalAnswer) {
                return jsonMap;
            } else {
                executeToolCall(jsonMap, emitter);
            }
            step++;
        }
        return Collections.emptyMap();
    }

    @SuppressWarnings("all")
    private String callLlm(SseEmitter emitter, AtomicBoolean interruptFlag, ChatClient chatClient, String question,
            boolean showThinking, String formatErrorFeedback) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        SecurityContext securityContext = SecurityContextHolder.getContext();
        Prompt prompt = getPrompt(question, showThinking, formatErrorFeedback);
        StringBuilder fullOutput = new StringBuilder();
        AtomicBoolean hasJsonOutput = new AtomicBoolean(false);
        AtomicReference<String> lastOutput = new AtomicReference<>("");

        CountDownLatch latch = new CountDownLatch(1);
        chatClient.prompt(prompt)
                .advisors(a -> a.params(
                        Map.of(
                                PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.THINK_ANSWER)))
                .stream()
                .chatResponse()
                .subscribe(chatResponse -> {
                    ChatContextHolder.setChatContext(chatContext);
                    SecurityContextHolder.setContext(securityContext);

                    Generation generation = chatResponse.getResult();
                    if (Objects.isNull(generation)) {
                        return;
                    }

                    String outputText = generation.getOutput().getText();
                    if (StringUtils.isNotEmpty(outputText)) {
                        fullOutput.append(outputText);
                        lastOutput.set(outputText);
                    }

                    // 检测是否包含 JSON 内容
                    if (containsJsonPattern(outputText)) {
                        hasJsonOutput.compareAndSet(false, true);
                    }

                    if (!hasJsonOutput.get() && showThinking && StringUtils.isNotEmpty(outputText)) {
                        if ("\n".equals(outputText) && lastOutput.get().equals("\n")) {
                            return;
                        }
                        SseUtil.sendChatBIThinking(emitter, outputText, false);
                    }
                }, error -> {
                    log.error("Error in chat response stream", error);
                    ChatContextHolder.setChatContext(chatContext);
                    SecurityContextHolder.setContext(securityContext);
                    SseUtil.sendChatBIError(emitter, "模型调用失败，请检查提供商配置和额度");
                    latch.countDown();
                }, latch::countDown);
        try {
            boolean completed = latch.await(5, TimeUnit.MINUTES);
            if (!completed) {
                log.error("Timed out waiting for chat response stream");
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.error("Interrupted waiting for chat response stream");
        }
        return fullOutput.toString();
    }

    private List<ToolDefinition> getToolDefinitions() {
        return CommonUtil.stream(methodTools)
                .filter(m -> !ExecutePythonTool.TOOL_NAME.equals(m.toolName()))
                .flatMap(
                        methodTool -> Arrays.stream(methodTool.getToolCallbacks()).map(ToolCallback::getToolDefinition))
                .toList();
    }

    private Prompt getPrompt(String question, boolean showThinking, String formatErrorFeedback) {
        // 获取会话历史用户消息
        List<String> historicalQuestions = ChatContextHolder.getChatContext().getHistoricalQuestions();

        // 获取上一轮的思考内容
        String previousThinking = ChatContextHolder.getChatContext().getPreviousThinking();

        // 获取示例 SQL
        List<Map<String, String>> sampleSqls = ChatContextHolder.getChatContext().getSampleSqls();

        var thinkAnswerPromptBuilder = promptTemplate.getTemplates()
                .get(PromptTemplate.THINK_ANSWER)
                .param("question", question)
                .param("raw_question", ChatContextHolder.getChatContext().getRawQuestion())
                .param("historical_questions", CollectionUtils.isEmpty(historicalQuestions)
                        ? new ArrayList<>()
                        : new ArrayList<>(historicalQuestions))
                .param("tool_definitions", getToolDefinitions())
                .param("tool_execution_results", ChatContextHolder.getChatContext().getToolCallResults())
                .param("previous_thinking", previousThinking != null ? previousThinking : "")
                .param("sample_sqls", CollectionUtils.isEmpty(sampleSqls) ? new ArrayList<>() : sampleSqls)
                .param("show_thinking", showThinking)
                .param("format_error_feedback", formatErrorFeedback);

        Prompt.Builder builder = Prompt.builder();
        builder.chatOptions(
                ToolCallingChatOptions.builder().internalToolExecutionEnabled(false).build());
        builder.messages(
                new SystemMessage(thinkAnswerPromptBuilder.buildSystemPrompt(PromptTemplate.THINK_ANSWER)),
                new UserMessage(thinkAnswerPromptBuilder.buildUserPrompt(PromptTemplate.THINK_ANSWER)));
        return builder.build();
    }

    /**
     * 执行工具调用
     *
     * @param toolCall
     *            工具调用参数，包含工具名称和参数
     * @param emitter
     *            SSE 事件发射器，用于发送工具调用结果
     * @return
     */
    @SuppressWarnings("all")
    private boolean executeToolCall(Map<String, Object> toolCall, SseEmitter emitter) {
        Map<String, Object> toolCallResult;

        // 从 tool_call 对象中提取 name 和 parameters
        Object toolCallObj = toolCall.get("tool_call");
        if (!(toolCallObj instanceof Map)) {
            toolCallResult = Map.of(
                    "error", "Invalid tool_call format. Expected an object with 'name' and 'parameters'.");
            setToolCallResult(toolCallResult);
            return false;
        }

        @SuppressWarnings("unchecked")
        Map<String, Object> toolCallMap = (Map<String, Object>) toolCallObj;
        Object toolNameObj = toolCallMap.get("name");
        Object parametersObj = toolCallMap.get("parameters");

        if (Objects.isNull(toolNameObj)) {
            toolCallResult = Map.of(
                    "error", "Tool name cannot be null, please check the tool name in the tool call and try again.");
            setToolCallResult(toolCallResult);
            return false;
        }

        if (Objects.isNull(parametersObj)) {
            toolCallResult = Map.of(
                    "error",
                    "Tool parameters cannot be null, please check the tool parameters in the tool call and try again.");
            setToolCallResult(toolCallResult);
            return false;
        }

        String toolName = toolNameObj.toString();
        String parameters = parametersObj.toString();

        String executeTime = LocalDateTime.now()
                .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS));
        boolean isAskUser = false;
        try {
            log.info("Executing tool: {}, parameters: {}", toolName, parameters);
            SseUtil.sendChatBIToolCall(emitter, "开始执行工具【%s】".formatted(toolName));

            Object tool = SpringContextUtil.getBean(toolName);
            Method executeMethod = Arrays.stream(tool.getClass().getDeclaredMethods()).filter(
                    method -> "execute".equals(method.getName())).findFirst().orElse(null);
            Class<?>[] executeMethodParamTypes = executeMethod.getParameterTypes();
            Object executeMethodResult;
            if (executeMethodParamTypes != null && executeMethodParamTypes.length > 0) {
                Map<String, Object> paramsMap = CommonUtil.nonJdkDeserializeObject(parameters,
                        new TypeReference<Map<String, Object>>() {
                        });
                Object request = CommonUtil.convertMap2Obj((Map<String, Object>) paramsMap.get("request"),
                        executeMethodParamTypes[0]);

                CommonUtil.validateBean(request);
                if (AskUserTool.TOOL_NAME.equals(toolName) || AnalyzeDataTool.TOOL_NAME.equals(toolName)) {
                    executeMethodResult = executeMethod.invoke(tool, request, emitter);
                } else {
                    executeMethodResult = executeMethod.invoke(tool, request);
                }
            } else {
                executeMethodResult = executeMethod.invoke(tool);
            }

            String result = CommonUtil.nonJdkSerializeObject(executeMethodResult);
            log.info("Tool {} executed: {}", toolName, result);

            toolCallResult = Map.of(
                    "tool_name", toolName,
                    "execute_time", executeTime,
                    "result", result);
            SseUtil.sendChatBIToolCall(emitter, "工具【%s】执行成功".formatted(toolName));
        } catch (Exception ex) {
            log.error("Error executing tool: {}", toolName, ex);
            String errorMsg = "Error: " + ex.getMessage();
            if (Objects.isNull(ex.getMessage()) && Objects.nonNull(ex.getCause())) {
                errorMsg = "Error: " + ex.getCause().getMessage();
            }

            if (ex.getCause() instanceof JacksonException) {
                errorMsg = errorMsg + ", Please check the tool parameters format. The invalid parameters are: "
                        + parameters;
            }

            if (ex.getCause() instanceof ValidationException vEx) {
                errorMsg = errorMsg + ", Please check the tool parameters. The invalid parameters are: "
                        + CommonUtil.stream(vEx.getConstraintViolations()).map(ConstraintViolation::getMessage)
                                .collect(Collectors.joining(CommonConstants.COMMA));
            }

            toolCallResult = Map.of(
                    "tool_name", toolName,
                    "execute_time", executeTime,
                    "result", errorMsg);
            SseUtil.sendChatBIToolCall(emitter, "工具【%s】执行失败".formatted(toolName));
        }
        setToolCallResult(toolCallResult);
        return isAskUser;
    }

    /**
     * 设置工具调用结果到上下文
     *
     * @param toolCallResult
     *            工具调用结果，包含工具名称、执行时间和结果
     */
    private void setToolCallResult(Map<String, Object> toolCallResult) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        if (CollectionUtils.isEmpty(chatContext.getToolCallResults())) {
            chatContext.setToolCallResults(new ArrayList<>());
        }
        chatContext.getToolCallResults().addFirst(toolCallResult);
    }

    /**
     * 保存思考内容到上下文，供下一轮推理使用 只保留上一轮的思考内容
     *
     * @param thinkingContent
     *            思考内容
     */
    private void saveThinkingContent(String thinkingContent) {
        ChatContextHolder.getChatContext().setPreviousThinking(thinkingContent);
    }

    /**
     * 验证 LLM 输出是否符合指定的格式要求
     *
     * @param llmResult
     *            LLM 原始输出
     * @param showThinking
     *            是否显示思考过程
     * @return 验证结果，包含是否有效和错误信息
     */
    @SuppressWarnings({"unchecked", "java:S3776"})
    private FormatValidationResult validateOutputFormat(String llmResult, boolean showThinking) {
        if (StringUtils.isEmpty(llmResult)) {
            return new FormatValidationResult(false, "LLM output is empty");
        }

        // 移除 JSON 格式中的 ```json 或 ``` 包裹
        if (llmResult.contains("```json")) {
            llmResult = llmResult.replace("```json", "");
        }
        if (llmResult.contains("```")) {
            llmResult = llmResult.replace("```", "");
        }

        // 检测是否包含 JSON
        int startIndex = llmResult.indexOf("{");
        int endIndex = llmResult.lastIndexOf("}");

        if (startIndex == -1 || endIndex == -1) {
            return new FormatValidationResult(false,
                    "Output does not contain valid JSON. Please output JSON in the required format.");
        }

        String jsonContent = llmResult.substring(startIndex, endIndex + 1);

        // 尝试解析 JSON
        Map<String, Object> jsonMap;
        try {
            jsonMap = CommonUtil.nonJdkDeserializeObject(jsonContent,
                    new TypeReference<Map<String, Object>>() {
                    });
        } catch (Exception e) {
            return new FormatValidationResult(false,
                    "Invalid JSON format. Please check your JSON syntax: " + e.getMessage());
        }

        String reason = llmResult.substring(0, startIndex).trim();
        if (showThinking) {
            // 验证思考内容格式
            // 检查是否有分隔符 ---
            if (!reason.contains("---")) {
                return new FormatValidationResult(false,
                        "Thinking content and JSON must be separated by '---'. Please output in format: 'thinking content --- {json}'");
            }
        } else {
            // 不显示思考过程的格式
            // 检查是否有思考内容（不应该有）
            if (StringUtils.isNotEmpty(reason) && !reason.equals("{")) {
                return new FormatValidationResult(false,
                        "When show_thinking is false, output should be JSON only without any text before it.");
            }
        }

        // 验证 JSON 格式 - 必须包含 need_next_step
        if (!jsonMap.containsKey("need_next_step")) {
            return new FormatValidationResult(false,
                    "JSON must contain 'need_next_step' field. Required format: { \"need_next_step\": true/false, ... }");
        }

        Boolean needNextStep = (Boolean) jsonMap.get("need_next_step");

        if (Boolean.TRUE.equals(needNextStep)) {
            // 需要继续，验证 tool_call 格式
            if (!jsonMap.containsKey("tool_call")) {
                return new FormatValidationResult(false,
                        "When need_next_step is true, JSON must contain 'tool_call' field with 'name' and 'parameters'.");
            }
            Object toolCall = jsonMap.get("tool_call");
            if (!(toolCall instanceof Map)) {
                return new FormatValidationResult(false, "'tool_call' must be an object.");
            }
            Map<String, Object> toolCallMap = (Map<String, Object>) toolCall;
            if (!toolCallMap.containsKey("name") || !toolCallMap.containsKey("parameters")) {
                return new FormatValidationResult(false,
                        "'tool_call' must contain 'name' and 'parameters' fields.");
            }
        } else {
            // 不需要继续，验证 final_answer 格式
            if (!jsonMap.containsKey("final_answer")) {
                return new FormatValidationResult(false,
                        "When need_next_step is false, JSON must contain 'final_answer' field.");
            }
        }

        return new FormatValidationResult(true, null, jsonMap);
    }

    /**
     * 检查文本是否包含 JSON 模式
     *
     * @param text
     *            待检查的文本
     * @return 如果文本包含 JSON 模式则返回 true，否则返回 false
     */
    @SuppressWarnings("java:S3776")
    private boolean containsJsonPattern(String text) {
        if (text == null) {
            return false;
        }

        // 排除代码块中的内容
        String[] lines = text.split("\n");
        for (String line : lines) {
            if (line.trim().startsWith("---")) {
                return true;
            }

            String trimmed = line.trim();

            if (trimmed.contains("{") || trimmed.contains("}")) {
                // 可能是 final_answer 或 tool call，视为 JSON
                return true;
            }

            // 检测 name:, parameters:, final_answer: 等模式
            if (trimmed.contains("\"name\"") || trimmed.contains("\"parameters\"")
                    || trimmed.contains("\"final_answer\"")) {
                return true;
            }
        }
        return false;
    }

    /**
     * 格式验证结果
     */
    @Getter
    private static class FormatValidationResult {
        private final boolean valid;
        private final String errorMessage;
        private final Map<String, Object> jsonMap;

        public FormatValidationResult(boolean valid, String errorMessage) {
            this(valid, errorMessage, null);
        }

        public FormatValidationResult(boolean valid, String errorMessage, Map<String, Object> jsonMap) {
            this.valid = valid;
            this.errorMessage = errorMessage;
            this.jsonMap = jsonMap;
        }

    }
}
