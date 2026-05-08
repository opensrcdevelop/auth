package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.entity.ChatMessageHistory;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.enums.ChatRole;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.ChatAnswerService;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ValidationException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.fasterxml.jackson.core.JacksonException;
import com.fasterxml.jackson.core.type.TypeReference;
import jakarta.validation.ConstraintViolation;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.AssistantMessage.ToolCall;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.messages.ToolResponseMessage;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.ai.chat.model.Generation;
import org.springframework.ai.model.tool.ToolCallingChatOptions;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

@Component
@RequiredArgsConstructor
@Slf4j
public class ThinkAnswerAgent {

    private static final Integer MAX_TOOL_MSG_LENGTH = 100;

    private final PromptTemplate promptTemplate;
    private final List<MethodTool> methodTools;
    private final ChatMessageHistoryService chatMessageHistoryService;
    private final ChatAnswerService chatAnswerService;

    /**
     * 执行思考回答的主入口方法
     * <p>
     * 该方法采用多步骤推理模式，通过循环调用 LLM 和执行工具来完成复杂任务。 每个步骤包括：调用 LLM 获取回复 → 检查是否有工具调用 → 执行工具 →
     * 返回结果
     * </p>
     *
     * @param emitter
     *            SSE Emitter，用于向客户端发送实时事件
     * @param interruptFlag
     *            中断标志，用于外部中断对话
     * @param chatClient
     *            ChatClient 实例，用于调用 LLM
     * @param maxSteps
     *            最大执行步数，防止无限循环
     * @param maxConsecutiveToolCalls
     *            连续工具调用阈值，超过后发送警告
     * @return 最终回答文本，如果被中断或达到最大步数则返回 null
     */
    @SuppressWarnings("java:S3776")
    public String thinkAnswer(SseEmitter emitter,
            AtomicBoolean interruptFlag,
            ChatClient chatClient,
            int maxSteps,
            int maxConsecutiveToolCalls) {
        ChatContext chatContext = ChatContextHolder.getChatContext();

        SseUtil.sendChatBILoading(emitter, "思考中...");
        List<Message> messages = buildMessages(chatContext.getChatId());
        String consecutiveToolCallWarning = null;
        int step = 0;

        while (step < maxSteps) {
            if (interruptFlag.get()) {
                log.info("ChatBI 对话（{}）被中断", chatContext.getChatId());
                break;
            }

            String stepThinkingMsg = step > 0
                    ? "\n<strong>Step " + (step + 1) + "</strong>\n"
                    : "<strong>Step " + (step + 1) + "</strong>\n";
            SseUtil.sendChatBIThinking(emitter, stepThinkingMsg, true);

            if (messages.getFirst() instanceof SystemMessage) {
                messages.set(0, new SystemMessage(buildSystemPrompt(consecutiveToolCallWarning)));
            } else {
                messages.addFirst(new SystemMessage(buildSystemPrompt(consecutiveToolCallWarning)));
            }
            AssistantMessage assistantMessage = callLlm(emitter, interruptFlag, chatClient, messages);
            if (interruptFlag.get()) {
                break;
            }

            messages.add(assistantMessage);
            boolean hasToolCalls = assistantMessage.hasToolCalls();
            String text = assistantMessage.getText();
            String reasoningContent = (String) assistantMessage.getMetadata().get("reasoningContent");

            if (StringUtils.isNotEmpty(reasoningContent)) {
                chatMessageHistoryService.createChatMessageHistory(reasoningContent, ChatContentType.THINKING);
            }

            if (hasToolCalls) {
                if (StringUtils.isNotEmpty(text)) {
                    SseUtil.sendChatBIThinking(emitter,
                            StringUtils.isNotEmpty(reasoningContent) ? "\n\n---\n\n" + text : text,
                            true);
                }

                List<ToolCall> toolCalls = assistantMessage.getToolCalls();
                if (CollectionUtils.isEmpty(toolCalls)) {
                    continue;
                }

                ToolCall toolCall = toolCalls.getFirst();
                String toolResult = executeToolCall(toolCall, emitter);
                ToolResponseMessage toolResponseMsg = ToolResponseMessage.builder()
                        .responses(List
                                .of(new ToolResponseMessage.ToolResponse(toolCall.id(), toolCall.name(), toolResult)))
                        .build();
                messages.add(toolResponseMsg);
                consecutiveToolCallWarning = buildConsecutiveToolCallsWarning(maxConsecutiveToolCalls);
                step++;
            } else {
                if (StringUtils.isNotEmpty(text)) {
                    return text;
                }
                log.warn("LLM 返回为空");
            }
        }

        return null;
    }

    /**
     * 调用 LLM 获取回复
     * <p>
     * 使用流式调用方式，通过 SSE 向客户端实时推送 LLM 的思考过程和输出内容。 收集完整的输出文本、推理内容和工具调用列表。
     * </p>
     *
     * @param emitter
     *            SSE Emitter，用于向客户端发送实时事件
     * @param interruptFlag
     *            中断标志，用于外部中断对话
     * @param chatClient
     *            ChatClient 实例
     * @param messages
     *            消息列表
     * @return AssistantMessage，包含完整输出、推理内容和工具调用
     */
    @SuppressWarnings("all")
    private AssistantMessage callLlm(SseEmitter emitter,
            AtomicBoolean interruptFlag,
            ChatClient chatClient,
            List<Message> messages) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        SecurityContext securityContext = SecurityContextHolder.getContext();

        CountDownLatch latch = new CountDownLatch(1);
        AtomicBoolean hasError = new AtomicBoolean(false);

        ToolCallback[] toolCallbacks = getToolCallbacks();
        StringBuilder fullOutput = new StringBuilder();
        StringBuilder reasoningContent = new StringBuilder();
        List<ToolCall> toolCallsList = new ArrayList<>();

        try {
            chatClient.prompt()
                    .messages(messages)
                    .options(ToolCallingChatOptions.builder()
                            .internalToolExecutionEnabled(false)
                            .toolCallbacks(toolCallbacks)
                            .build())
                    .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.THINK_ANSWER))
                    .stream()
                    .chatResponse()
                    .subscribe(chatResponse -> {
                        ChatContextHolder.setChatContext(chatContext);
                        SecurityContextHolder.setContext(securityContext);

                        Generation generation = chatResponse.getResult();
                        if (generation == null) {
                            return;
                        }

                        AssistantMessage assistantMessage = generation.getOutput();

                        String outputText = assistantMessage.getText();
                        if (StringUtils.isNotEmpty(outputText)) {
                            fullOutput.append(outputText);
                        }

                        String reasoning = (String) assistantMessage.getMetadata().get("reasoningContent");
                        if (StringUtils.isNotEmpty(reasoning)) {
                            SseUtil.sendChatBIThinking(emitter, reasoning, false);
                            reasoningContent.append(reasoning);
                        }

                        if (chatResponse.hasToolCalls()) {
                            List<ToolCall> calls = generation.getOutput().getToolCalls();
                            if (!calls.isEmpty()) {
                                toolCallsList.add(calls.getFirst());
                            }
                        }
                    }, error -> {
                        log.error("LLM 调用出错", error);
                        if (error.getCause().getCause() instanceof WebClientResponseException responseException) {
                            log.error("LLM Response Body: {}", responseException.getResponseBodyAsString());
                        }

                        ChatContextHolder.setChatContext(chatContext);
                        SecurityContextHolder.setContext(securityContext);
                        SseUtil.sendChatBIError(emitter, "模型调用失败，请检查提供商配置和额度");
                        hasError.set(true);
                        interruptFlag.set(true);
                        latch.countDown();
                    }, latch::countDown);

            boolean completed = latch.await(5, TimeUnit.MINUTES);
            if (!completed) {
                log.error("LLM 调用超时");
                interruptFlag.set(true);
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            log.error("LLM 调用被中断");
        }

        return AssistantMessage.builder()
                .content(fullOutput.toString())
                .properties(Map.of("reasoningContent", reasoningContent.toString()))
                .toolCalls(toolCallsList)
                .build();
    }

    /**
     * 构建系统提示词
     *
     * @param consecutiveToolCallWarning
     *            连续工具调用警告信息，如果为 null 则不包含警告
     * @return 构建后的系统提示词
     */
    private String buildSystemPrompt(String consecutiveToolCallWarning) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        var thinkAnswerPromptBuilder = promptTemplate.getTemplates().get(PromptTemplate.THINK_ANSWER);
        var sampleSqls = chatContext.getSampleSqls();
        var extraInstruction = chatContext.getChatConfig() != null
                ? chatContext.getChatConfig().getExtraInstruction()
                : null;
        var systemTime = java.time.LocalDateTime.now().format(
                java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        return thinkAnswerPromptBuilder
                .param("consecutive_tool_call_warning",
                        consecutiveToolCallWarning != null ? consecutiveToolCallWarning : "")
                .param("sample_sqls", CollectionUtils.isEmpty(sampleSqls) ? new ArrayList<>() : sampleSqls)
                .param("extra_instruction", extraInstruction != null ? extraInstruction : "")
                .param("system_time", systemTime)
                .buildSystemPrompt(PromptTemplate.THINK_ANSWER);
    }

    /**
     * 构建消息列表
     * <p>
     * 组装完整的对话上下文，包括：系统提示、历史用户消息和AI回复、当前用户问题。 用于发送给 LLM 进行推理。
     * </p>
     *
     * @param chatId
     *            对话 ID
     * @return 消息列表
     */
    private List<Message> buildMessages(String chatId) {
        List<Message> messages = new ArrayList<>();
        ChatContext chatContext = ChatContextHolder.getChatContext();

        // 添加系统提示
        messages.add(new SystemMessage(buildSystemPrompt(null)));

        // 获取用户历史消息
        List<ChatMessageHistory> userMessages = chatMessageHistoryService.list(
                Wrappers.<ChatMessageHistory>lambdaQuery()
                        .eq(ChatMessageHistory::getChatId, chatId)
                        .eq(ChatMessageHistory::getRole, ChatRole.USER.name())
                        .isNotNull(ChatMessageHistory::getContent)
                        .orderByAsc(ChatMessageHistory::getCreateTime));
        // 移除当前问题
        if (!userMessages.isEmpty()) {
            userMessages.removeLast();
        }

        if (!userMessages.isEmpty()) {
            // 获取对应的回答
            List<String> questionIds = userMessages.stream()
                    .map(ChatMessageHistory::getQuestionId)
                    .filter(StringUtils::isNotBlank)
                    .distinct()
                    .collect(Collectors.toList());

            Map<String, ChatAnswer> answerMap = new HashMap<>();
            if (!questionIds.isEmpty()) {
                List<ChatAnswer> answers = chatAnswerService.list(
                        Wrappers.<ChatAnswer>lambdaQuery()
                                .eq(ChatAnswer::getChatId, chatId)
                                .in(ChatAnswer::getQuestionId, questionIds));
                for (ChatAnswer answer : answers) {
                    answerMap.put(answer.getQuestionId(), answer);
                }
            }

            // 组装历史对话
            for (ChatMessageHistory userMsg : userMessages) {
                messages.add(new UserMessage(userMsg.getContent()));
                ChatAnswer chatAnswer = answerMap.get(userMsg.getQuestionId());
                if (chatAnswer != null && StringUtils.isNotBlank(chatAnswer.getAnswer())) {
                    messages.add(new AssistantMessage(chatAnswer.getAnswer()));
                } else {
                    messages.add(new AssistantMessage(""));
                }
            }
        }

        // 添加当前用户问题
        messages.add(new UserMessage(chatContext.getRawQuestion()));
        return messages;
    }

    /**
     * 获取工具回调数组
     *
     * @return ToolCallback 数组
     */
    private ToolCallback[] getToolCallbacks() {
        return CommonUtil.stream(methodTools)
                .filter(m -> !m.isInternalTool())
                .flatMap(m -> Arrays.stream(m.getToolCallbacks()))
                .toArray(ToolCallback[]::new);
    }

    /**
     * 执行工具调用
     * <p>
     * 根据 LLM 返回的工具调用信息，查找并执行对应的工具。 处理工具执行结果和异常情况。
     * </p>
     *
     * @param toolCall
     *            工具调用信息
     * @param emitter
     *            SSE Emitter，用于发送工具执行状态
     * @return 工具执行结果（成功时返回结果字符串，失败时返回错误信息）
     */
    @SuppressWarnings("unchecked")
    private String executeToolCall(ToolCall toolCall, SseEmitter emitter) {
        String toolName = toolCall.name();
        String parameters = toolCall.arguments();

        try {
            log.info("Executing tool: {}, parameters: {}", toolName, parameters);
            SseUtil.sendChatBIToolCall(emitter,
                    "开始执行工具【%s】，参数：%s".formatted(toolName, truncateString(parameters)));

            MethodTool methodTool = methodTools.stream()
                    .filter(m -> m.toolName().equals(toolName))
                    .findFirst()
                    .orElseThrow(() -> new IllegalArgumentException("Tool not found: " + toolName));

            ToolCallback[] callbacks = methodTool.getToolCallbacks();
            if (callbacks == null || callbacks.length == 0) {
                throw new IllegalStateException("Tool " + toolName + " has no available ToolCallback");
            }

            ToolCallback callback = callbacks[0];

            Class<?>[] paramTypes = getToolMethodParamTypes(toolName);
            if (paramTypes.length > 0) {
                Map<String, Object> paramsMap = CommonUtil.nonJdkDeserializeObject(parameters,
                        new TypeReference<Map<String, Object>>() {
                        });
                Map<String, Object> params;
                if (paramsMap.containsKey("request")) {
                    params = (Map<String, Object>) paramsMap.get("request");
                } else {
                    params = paramsMap;
                }
                Object request = CommonUtil.convertMap2Obj(params, paramTypes[0]);
                CommonUtil.validateBean(request);
            }

            Object result = callback.call(parameters);
            String resultStr = CommonUtil.nonJdkSerializeObject(result);
            log.info("Tool {} executed successfully: {}", toolName, resultStr);
            SseUtil.sendChatBIToolCall(emitter, "工具【%s】执行成功，结果：%s".formatted(toolName, truncateString(resultStr)));

            updateConsecutiveToolCalls(toolName);
            return resultStr;
        } catch (Exception ex) {
            log.error("Tool {} execution failed", toolName, ex);
            SseUtil.sendChatBIToolCall(emitter, "工具【%s】执行失败".formatted(toolName));

            String errorMsg = "Error: " + ex.getMessage();
            if (Objects.isNull(ex.getMessage()) && Objects.nonNull(ex.getCause())) {
                errorMsg = "Error: " + ex.getCause().getMessage();
            }

            if (ex.getCause() instanceof JacksonException) {
                errorMsg = errorMsg + ". Please check the tool parameters format. Invalid parameters: " + parameters;
            }

            if (ex.getCause() instanceof ValidationException vEx) {
                errorMsg = errorMsg + ". Please check the tool parameters. Invalid parameters: " +
                        CommonUtil.stream(vEx.getConstraintViolations())
                                .map(ConstraintViolation::getMessage)
                                .collect(Collectors.joining(CommonConstants.COMMA));
            }

            updateConsecutiveToolCalls(toolName);
            return errorMsg;
        }
    }

    /**
     * 获取工具方法的参数类型
     *
     * @param toolName
     *            工具名称
     * @return 参数类型数组
     */
    private Class<?>[] getToolMethodParamTypes(String toolName) {
        try {
            Object tool = SpringContextUtil.getBean(toolName);
            java.lang.reflect.Method executeMethod = Arrays.stream(tool.getClass().getDeclaredMethods())
                    .filter(method -> "execute".equals(method.getName()))
                    .findFirst()
                    .orElse(null);
            if (executeMethod != null) {
                return executeMethod.getParameterTypes();
            }
        } catch (Exception e) {
            log.warn("Failed to get method parameter types for tool {}", toolName, e);
        }
        return new Class[0];
    }

    /**
     * 更新连续工具调用计数
     *
     * @param toolName
     *            工具名称
     */
    private void updateConsecutiveToolCalls(String toolName) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        if (toolName == null) {
            return;
        }
        if (toolName.equals(chatContext.getLastToolCallName())) {
            chatContext.setConsecutiveToolCalls(chatContext.getConsecutiveToolCalls() + 1);
        } else {
            chatContext.setConsecutiveToolCalls(1);
        }
        chatContext.setLastToolCallName(toolName);
    }

    /**
     * 构建连续工具调用警告信息
     *
     * @param maxConsecutiveToolCalls
     *            最大允许的连续工具调用次数
     * @return 警告信息，如果未达到阈值则返回 null
     */
    private String buildConsecutiveToolCallsWarning(int maxConsecutiveToolCalls) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Integer consecutiveCalls = chatContext.getConsecutiveToolCalls();
        String lastTool = chatContext.getLastToolCallName();

        if (consecutiveCalls == null || consecutiveCalls < maxConsecutiveToolCalls) {
            return null;
        }

        return String.format(
                "WARNING: Tool '%s' has been called %d consecutive times (threshold: %d). "
                        + "Please reconsider your strategy - verify if this tool is appropriate for the current task, "
                        + "check the parameters format, and consider using an alternative approach if needed. "
                        + "If this tool continues to be called repeatedly, consider providing a final answer based on available data.",
                lastTool, consecutiveCalls, maxConsecutiveToolCalls);
    }

    /**
     * 截断字符串到最大长度
     *
     * @param str
     *            原字符串
     * @return 截断后的字符串，如果超过最大长度则添加 "..."
     */
    private String truncateString(String str) {
        if (StringUtils.isBlank(str)) {
            return str;
        }

        if (str.length() <= MAX_TOOL_MSG_LENGTH) {
            return str;
        }

        return str.substring(0, MAX_TOOL_MSG_LENGTH) + "...";
    }
}
