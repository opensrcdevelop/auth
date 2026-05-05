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

            messages.addFirst(new SystemMessage(buildSystemPrompt(consecutiveToolCallWarning)));
            AssistantMessage assistantMessage = callLlm(emitter, interruptFlag, chatClient, messages);
            if (interruptFlag.get()) {
                break;
            }

            messages.add(assistantMessage);
            boolean hasToolCalls = assistantMessage.hasToolCalls();
            String text = assistantMessage.getText();
            if (hasToolCalls) {
                if (StringUtils.isNotEmpty(text)) {
                    SseUtil.sendChatBIThinking(emitter, text, true);
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

                        String outputText = generation.getOutput().getText();
                        if (StringUtils.isNotEmpty(outputText)) {
                            fullOutput.append(outputText);
                        }

                        if (chatResponse.hasToolCalls()) {
                            List<ToolCall> calls = generation.getOutput().getToolCalls();
                            if (!calls.isEmpty()) {
                                toolCallsList.addAll(calls);
                            }
                        }
                    }, error -> {
                        log.error("LLM 调用出错", error);
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
                .toolCalls(toolCallsList)
                .build();
    }

    private String buildSystemPrompt(String consecutiveToolCallWarning) {
        var thinkAnswerPromptBuilder = promptTemplate.getTemplates().get(PromptTemplate.THINK_ANSWER);
        var sampleSqls = ChatContextHolder.getChatContext().getSampleSqls();
        return thinkAnswerPromptBuilder
                .param("consecutive_tool_call_warning",
                        consecutiveToolCallWarning != null ? consecutiveToolCallWarning : "")
                .param("sample_sqls", CollectionUtils.isEmpty(sampleSqls) ? new ArrayList<>() : sampleSqls)
                .buildSystemPrompt(PromptTemplate.THINK_ANSWER);
    }

    private List<Message> buildMessages(String chatId) {
        List<Message> messages = new ArrayList<>();
        ChatContext chatContext = ChatContextHolder.getChatContext();

        // 添加系统提示
        messages.add(new SystemMessage(buildSystemPrompt(null)));

        // 获取用户历史消息
        List<ChatMessageHistory> userMessages = chatMessageHistoryService.list(
                com.baomidou.mybatisplus.core.toolkit.Wrappers.<ChatMessageHistory>lambdaQuery()
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
                        com.baomidou.mybatisplus.core.toolkit.Wrappers.<ChatAnswer>lambdaQuery()
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
                }
            }
        }

        // 添加当前用户问题
        messages.add(new UserMessage(chatContext.getRawQuestion()));
        return messages;
    }

    private ToolCallback[] getToolCallbacks() {
        return CommonUtil.stream(methodTools)
                .filter(m -> !m.isInternalTool())
                .flatMap(m -> Arrays.stream(m.getToolCallbacks()))
                .toArray(ToolCallback[]::new);
    }

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
