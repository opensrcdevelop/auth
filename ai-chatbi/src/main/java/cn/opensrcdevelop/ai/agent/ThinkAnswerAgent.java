package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import com.fasterxml.jackson.core.JacksonException;
import com.fasterxml.jackson.core.type.TypeReference;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.messages.UserMessage;
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
     * @param emitter       SSE
     * @param interruptFlag 中断标志
     * @param chatClient    ChatClient
     * @param userQuestion  用户提问
     * @param maxSteps      最大执行步数
     */
    public Map<String, Object> thinkAnswer(SseEmitter emitter,
                                           AtomicBoolean interruptFlag,
                                           ChatClient chatClient,
                                           String userQuestion,
                                           int maxSteps) {
        SseUtil.sendChatBILoading(emitter, "思考中...");
        int step = 0;
        while (step < maxSteps) {
            if (interruptFlag.get()) {
                log.info("ChatBI 对话（{}）被中断", ChatContextHolder.getChatContext().getChatId());
                break;
            }

            String stepThinkingMsg = step > 0 ? "\n<strong>Step " + (step + 1) + "</strong>\n" : "<strong>Step " + (step + 1) + "</strong>\n";
            SseUtil.sendChatBIThinking(emitter, stepThinkingMsg, true);

            String result = callLlm(emitter, chatClient, step > 0 ? null : userQuestion);
            var parseResult = parseLlmResult(result);
            boolean isFinalAnswer = result.contains("final_answer");
            chatMessageHistoryService.createChatMessageHistory(parseResult._1(), ChatContentType.THINKING);
            if (isFinalAnswer) {
                return parseResult._2();
            } else {
                executeToolCall(parseResult._2(), emitter);
            }
            step++;
        }
        return Collections.emptyMap();
    }

    private String callLlm(SseEmitter emitter, ChatClient chatClient, String question) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        SecurityContext securityContext = SecurityContextHolder.getContext();
        Prompt prompt = getPrompt(question);
        StringBuilder fullOutput = new StringBuilder();
        AtomicBoolean hasJsonOutput = new AtomicBoolean(false);

        CountDownLatch latch = new CountDownLatch(1);
        chatClient.prompt(prompt)
                .advisors(a -> a.params(
                        Map.of(
                                PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.THINK_ANSWER
                        )
                ))
                .stream()
                .chatResponse()
                .subscribe(chatResponse -> {
                    ChatContextHolder.setChatContext(chatContext);
                    SecurityContextHolder.setContext(securityContext);
                    String outputText = chatResponse.getResult().getOutput().getText();
                    fullOutput.append(outputText);
                    if (StringUtils.containsIgnoreCase(outputText, "```")) {
                        hasJsonOutput.compareAndSet(false, true);
                    }

                    if (!hasJsonOutput.get()) {
                        SseUtil.sendChatBIThinking(emitter, outputText, false);
                    }
                }, error -> {
                    log.error("Error in chat response stream", error);
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
                .flatMap(methodTool -> Arrays.stream(methodTool.getToolCallbacks()).map(ToolCallback::getToolDefinition)).toList();
    }

    private Prompt getPrompt(String question) {
        var thinkAnswerPrompt = promptTemplate.getTemplates()
                .get(PromptTemplate.THINK_ANSWER)
                .param("question", question)
                .param("tool_definitions", getToolDefinitions())
                .param("tool_execution_results", ChatContextHolder.getChatContext().getToolCallResults());
        Prompt.Builder builder = Prompt.builder();
        builder.chatOptions(
                ToolCallingChatOptions.builder().internalToolExecutionEnabled(false).build()
        );
        builder.messages(
                new SystemMessage(thinkAnswerPrompt.buildSystemPrompt(PromptTemplate.THINK_ANSWER)),
                new UserMessage(thinkAnswerPrompt.buildUserPrompt(PromptTemplate.THINK_ANSWER))
        );
        return builder.build();
    }

    @SuppressWarnings("all")
    private void executeToolCall(Map<String, Object> toolCall, SseEmitter emitter) {
        Map<String, Object> toolCallResult;
        String toolName = toolCall.get("name").toString();
        String parameters = toolCall.get("parameters").toString();
        String executeTime = LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS));
        try {
            log.info("Executing tool: {}, parameters: {}", toolName, parameters);
            String startThinkMsg = "\n%s - 开始执行工具【%s】\n".formatted(
                    LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)),
                    toolName);
            SseUtil.sendChatBIThinking(emitter, startThinkMsg, true);

            Object tool = SpringContextUtil.getBean(toolName);
            Method executeMethod = Arrays.stream(tool.getClass().getDeclaredMethods()).filter(
                    method -> "execute".equals(method.getName())
            ).findFirst().orElse(null);
            Class<?>[] executeMethodParamTypes = executeMethod.getParameterTypes();
            Object executeMethodResult;
            if (executeMethodParamTypes != null && executeMethodParamTypes.length > 0) {
                Map<String, Object> paramsMap = CommonUtil.nonJdkDeserializeObject(parameters, new TypeReference<Map<String, Object>>() {
                });
                Object request = CommonUtil.convertMap2Obj((Map<String, Object>) paramsMap.get("request"), executeMethodParamTypes[0]);
                executeMethodResult = executeMethod.invoke(tool, request);
            } else {
                executeMethodResult = executeMethod.invoke(tool);
            }

            String result = CommonUtil.nonJdkSerializeObject(executeMethodResult);
            log.info("Tool {} executed: {}", toolName, result);

            toolCallResult = Map.of(
                    "tool_name", toolName,
                    "execute_time", executeTime,
                    "result", result
            );

            String endThinkingMsg = "%s - 工具【%s】执行成功\n".formatted(
                    LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)),
                    toolName
            );
            SseUtil.sendChatBIThinking(emitter, endThinkingMsg, true);
        } catch (Exception ex) {
            log.error("Error executing tool: {}", toolName, ex);
            String errorMsg = "Error: " + ex.getMessage();
            if (Objects.isNull(ex.getMessage()) && Objects.nonNull(ex.getCause())) {
                errorMsg = "Error: " + ex.getCause().getMessage();
            }

            if (ex.getCause() instanceof JacksonException) {
                errorMsg = errorMsg + ", Please check the tool parameters format.";
            }
            toolCallResult = Map.of(
                    "tool_name", toolName,
                    "execute_time", executeTime,
                    "result", errorMsg
            );

            String errorThinkingMsg = "%s - 工具【%s】执行失败\n".formatted(
                    LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)),
                    toolName
            );
            SseUtil.sendChatBIThinking(emitter, errorThinkingMsg, true);
        }
        ChatContext chatContext = ChatContextHolder.getChatContext();
        if (CollectionUtils.isEmpty(chatContext.getToolCallResults())) {
            chatContext.setToolCallResults(new ArrayList<>());
        }
        chatContext.getToolCallResults().addFirst(toolCallResult);
    }

    private Tuple2<String, Map<String, Object>> parseLlmResult(String llmResult) {
        int startIndex = llmResult.indexOf("{");
        int endIndex = llmResult.lastIndexOf("}");

        if (startIndex == -1 || endIndex == -1) {
            return Tuple.of("", Collections.emptyMap());
        }

        String reason = llmResult.substring(0, startIndex);
        if (reason.contains("```json")) {
            reason = reason.replace("```json", "");
        }

        String json = llmResult.substring(startIndex, endIndex + 1);
        Map<String, Object> toolCall = CommonUtil.nonJdkDeserializeObject(json, new TypeReference<Map<String, Object>>() {
        });
        return Tuple.of(reason, toolCall);
    }
}
