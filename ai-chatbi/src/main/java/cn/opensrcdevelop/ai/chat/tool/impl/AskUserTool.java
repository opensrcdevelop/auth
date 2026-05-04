package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.constants.RedisTopicConstants;
import cn.opensrcdevelop.ai.dto.UserAnswerDto;
import cn.opensrcdevelop.ai.dto.UserAnswerRequestDto;
import cn.opensrcdevelop.ai.enums.QuestionType;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

@Slf4j
@Component(AskUserTool.TOOL_NAME)
@RequiredArgsConstructor
public class AskUserTool implements MethodTool {

    public static final String TOOL_NAME = "ask_user";

    @SuppressWarnings("all")
    @Tool(name = TOOL_NAME, description = "Used when AI cannot answer directly or needs more information." +
            " Apply when: 1. Missing key filter conditions; 2. User intent is unclear;" +
            " 3. User needs to choose from multiple options;")
    public Response execute(@ToolParam(description = "Request parameters") Request request) {
        SseEmitter emitter = ChatContextHolder.getChatContext().getEmitter();
        Response response = new Response();
        if (CollectionUtils.isEmpty(request.getQuestions())) {
            response.setSuccess(false);
            response.setError("Questions cannot be empty, please provide at least one question.");
            return response;
        }

        // 1. 设置提问 ID
        CommonUtil.stream(request.questions).forEach(q -> q.setId(UUID.randomUUID().toString()));

        // 2. 向用户发送提问
        SseUtil.sendChatBILoading(emitter, "等待用户回答...");
        SseUtil.sendChatBIAskUser(emitter, request.getQuestions());

        // 3. 等待用户回答（2 分钟）
        ChatContext chatContext = ChatContextHolder.getChatContext();
        CountDownLatch latch = new CountDownLatch(1);
        AtomicReference<UserAnswerRequestDto> answerRef = new AtomicReference<>();

        int listenerId = RedisUtil.subscribeMessage(RedisTopicConstants.getTopic(chatContext.getChatId()),
                UserAnswerRequestDto.class, (x, message) -> {
                    answerRef.set(message);
                    latch.countDown();
                });

        try {
            boolean received = latch.await(2, TimeUnit.MINUTES);
            RedisUtil.removeListener(RedisTopicConstants.getTopic(chatContext.getChatId()), listenerId);

            if (!received || answerRef.get() == null) {
                log.warn("User did not answer in time, timeout.");

                response.setSuccess(false);
                response.setError("User did not answer in time, timeout.");
                return response;
            }
        } catch (InterruptedException e) {
            log.error("Wait for user answer interrupted", e);
            Thread.currentThread().interrupt();
        }

        // 4. 处理用户回答
        List<UserAnswerDto> userAnswers = answerRef.get().getAnswers();
        if (CollectionUtils.isEmpty(userAnswers)) {
            response.setSuccess(true);
            response.setError("User did not answer any question.");
            return response;
        }

        response.setSuccess(true);
        response.setAnswers(CommonUtil.stream(userAnswers).map(a -> {
            String questionText = CommonUtil.stream(request.getQuestions())
                    .filter(q -> q.getId().equals(a.getQuestionId())).findFirst().map(Question::getQuestionText)
                    .orElse(null);
            String answerText = a.getAnswer();

            Answer answer = new Answer();
            answer.setQuestionId(a.getQuestionId());
            answer.setQuestionText(questionText);
            answer.setAnswerText(answerText);

            String thinkingMsg = "\n%s - 收到问题【%s】的回答: %s\n".formatted(
                    LocalDateTime.now()
                            .format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)),
                    questionText, answerText);
            SseUtil.sendChatBIThinking(emitter, thinkingMsg, true);

            return answer;
        }).toList());
        return response;
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @Data
    public static class Request {

        @ToolParam(description = "List of questions, supports multiple questions (switchable via tab)", required = true)
        private List<Question> questions;
    }

    @Data
    public static class Question {
        private String id;

        @ToolParam(description = "The question to ask the user")
        @NotBlank
        private String questionText;

        @ToolParam(description = "Question type: TEXT, SELECT, MULTI_SELECT")
        @NotNull
        private QuestionType questionType;

        @ToolParam(description = "List of options, required when questionType is SELECT or MULTI_SELECT", required = false)
        private List<String> options;

        @ToolParam(description = "Whether the question is required, defaults to true")
        @NotNull
        private Boolean required;

        @ToolParam(description = "Context information to help user understand the question", required = false)
        private String context;

        @ToolParam(description = "Question title (short)")
        @NotBlank
        private String title;
    }

    @Data
    public static class Answer {

        @ToolParam(description = "Question ID")
        private String questionId;

        @ToolParam(description = "The question to ask the user")
        private String questionText;

        @ToolParam(description = "The user's answer")
        private String answerText;
    }

    @Data
    public static class Response {
        private Boolean success;

        private List<Answer> answers;

        private String error;
    }
}
