package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.agent.ThinkAnswerAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.client.ChatClientManager;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.dto.VoteAnswerRequestDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.exception.ValidationException;
import cn.opensrcdevelop.common.response.ValidationErrorResponse;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.MessageUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.zaxxer.hikari.pool.HikariPool;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.control.Try;
import jakarta.annotation.Resource;
import java.io.IOException;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

@Slf4j
@Service
@RequiredArgsConstructor
public class ChatBIServiceImpl implements ChatBIService {

    private static final Long CHAT_TIMEOUT = Duration.ofMinutes(60).toMillis();

    private final MessageUtil messageUtil;
    private final ChatAnswerService chatAnswerService;
    private final ChatClientManager chatClientManager;
    private final DataSourceConfService dataSourceConfService;
    private final ThinkAnswerAgent thinkAnswerAgent;
    private final ChatMessageHistoryService chatMessageHistoryService;
    private final ChatHistoryService chatHistoryService;

    @Resource(name = ExecutorConstants.EXECUTOR_IO_DENSE)
    private Executor executor;

    /**
     * ChatBI 用户对话
     *
     * @param requestDto
     *            请求
     * @return SseEmitter
     */
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.CHAT_BI, userOperation = UserOperationType.CHAT_BI_CHAT, success = "开启了 ChatBI 对话（ChatID：{{ #chatId }}），问题（ID：{{ #requestDto.questionId }}）：{{ #requestDto.question }}", fail = "开启 ChatBI 对话失败（ChatID：{{ #chatId }}），问题（ID：{{ #requestDto.questionId }}）：{{ #requestDto.question }}")
    @Override
    public SseEmitter streamChatBI(ChatBIRequestDto requestDto) {
        SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);
        AtomicBoolean interruptFlag = new AtomicBoolean(false);
        SecurityContext securityContext = SecurityContextHolder.getContext();

        if (isRequestInvalid(emitter, requestDto)) {
            return emitter;
        }

        String chatId = requestDto.getChatId();
        if (StringUtils.isEmpty(chatId)) {
            chatId = CommonUtil.getUUIDV7String();
            chatHistoryService.createChatHistory(chatId, requestDto.getQuestion(), requestDto.getDataSourceId());
        }
        AuditContext.setSpelVariable("chatId", chatId);
        String finalChatId = chatId;

        executor.execute(() -> {
            SecurityContextHolder.setContext(securityContext);
            ChatContext chatContext = new ChatContext();
            try {
                chatContext.setChatId(finalChatId);
                chatContext.setQuestionId(requestDto.getQuestionId());
                chatContext.setDataSourceId(requestDto.getDataSourceId());
                chatContext.setQuestion(requestDto.getQuestion());
                chatContext.setRawQuestion(requestDto.getQuestion());
                ChatContextHolder.setChatContext(chatContext);
                chatMessageHistoryService.createUserChatMessageHistory(requestDto.getQuestion());

                Tuple2<String, String> result = processStreamChatBIRequest(emitter, interruptFlag, requestDto,
                        finalChatId);
                if (!interruptFlag.get()) {
                    SseUtil.sendChatBIDone(emitter, result._1, result._2);
                } else {
                    chatMessageHistoryService.createChatMessageHistory("回答已取消", ChatContentType.LOADING);
                }
            } catch (HikariPool.PoolInitializationException ex) {
                SseUtil.sendChatBIError(emitter, messageUtil.getMsg(MessageConstants.AI_DATASOURCE_MSG_1003));
            } catch (Exception ex) {
                log.error(ex.getMessage(), ex);
                SseUtil.sendChatBIError(emitter, messageUtil.getMsg(MessageConstants.AI_CHAT_MSG_1000));
            } finally {
                emitter.complete();
                ChatContextHolder.removeChatContext(finalChatId);
            }
        });

        emitter.onCompletion(() -> {
            log.info("ChatBI 对话（{}）中断/结束", finalChatId);
            interruptFlag.set(true);
        });

        return emitter;
    }

    /**
     * 投票回答
     *
     * @param requestDto
     *            请求
     */
    @Audit(type = AuditType.USER_OPERATION, resource = ResourceType.CHAT_BI, userOperation = UserOperationType.CHAT_BI_VOTE, success = "反馈了 ChatBI 回答（ID：{{ #requestDto.answerId }}），反馈：{{ #requestDto.feedback }}", fail = "反馈 ChatBI 回答（ID：{{ #requestDto.answerId }}）失败，反馈：{{ #requestDto.feedback }}")
    @Override
    public void voteAnswer(VoteAnswerRequestDto requestDto) {
        // 1. 数据库操作
        chatAnswerService.update(Wrappers.<ChatAnswer>lambdaUpdate()
                .eq(ChatAnswer::getAnswerId, requestDto.getAnswerId())
                .set(ChatAnswer::getFeedback,
                        requestDto.getFeedback() == null ? null : requestDto.getFeedback().name()));
    }

    @SuppressWarnings("all")
    private Tuple2<String, String> processStreamChatBIRequest(SseEmitter emitter,
            AtomicBoolean interruptFlag,
            ChatBIRequestDto requestDto,
            String chatId) throws IOException {
        String dataSourceId = requestDto.getDataSourceId();
        String question = requestDto.getQuestion();

        // 1. 检查数据源是否已同步
        if (Boolean.FALSE.equals(dataSourceConfService.isSynced(dataSourceId))) {
            SseUtil.sendChatBIText(emitter, "数据源未同步，请先执行同步表操作。");
            return Tuple.of(null, question);
        }

        // 2. 获取 ChatClient
        ChatClient chatClient = chatClientManager.getChatClient(requestDto.getModelProviderId(), requestDto.getModel(),
                chatId);
        ChatContextHolder.getChatContext().setChatClient(chatClient);

        // 3. 回答问题
        SseUtil.sendChatBILoading(emitter, "正在回答问题...");
        Map<String, Object> answer = thinkAnswerAgent.thinkAnswer(
                emitter,
                interruptFlag,
                chatClient,
                question,
                30);

        if (interruptFlag.get()) {
            return Tuple.of(null, question);
        }

        if (MapUtils.isEmpty(answer)) {
            SseUtil.sendChatBIText(emitter, "抱歉无法回答您的提问，请稍后重试。");
            return Tuple.of(null, question);
        }
        String answerId = CommonUtil.getUUIDV7String();
        ChatAnswer chatAnswer = new ChatAnswer();
        chatAnswer.setAnswerId(answerId);
        chatAnswer.setModelProviderId(requestDto.getModelProviderId());
        chatAnswer.setModel(requestDto.getModel());
        chatAnswer.setDataSourceId(dataSourceId);
        chatAnswer.setChatId(chatId);
        chatAnswer.setQuestionId(requestDto.getQuestionId());
        chatAnswer.setQuestion(requestDto.getQuestion());
        chatAnswer.setSql(ChatContextHolder.getChatContext().getSql());
        chatAnswer.setReqTokens(ChatContextHolder.getChatContext().getReqTokens().get());
        chatAnswer.setRepTokens(ChatContextHolder.getChatContext().getRepTokens().get());

        // 3.1 直接回答
        if (answer.containsKey("final_answer")) {
            String answerText = (String) answer.get("final_answer");
            chatAnswer.setAnswer(answerText);
            SseUtil.sendChatBITextSegmented(emitter, answerText, ChatContentType.MARKDOWN, 500);
        }

        // 3.2 图表
        if (answer.containsKey("chart") && Boolean.TRUE.equals(answer.get("chart"))) {

            // 3.2.1 生成图表
            Map<String, Object> chartConfig = ChatContextHolder.getChatContext().getChartConfig();
            chatAnswer.setChartConfig(CommonUtil.serializeObject(chartConfig));
            var renderResult = ChartRenderer.render(chartConfig, ChatContextHolder.getChatContext().getQueryData());
            if ("table".equals(renderResult._1)) {
                SseUtil.sendChatBITable(emitter, renderResult._2);
            } else {
                SseUtil.sendChatBIChart(emitter, renderResult._2);
            }
        }

        // 3.3 报告
        if (answer.containsKey("report") && Boolean.TRUE.equals(answer.get("report"))) {

            SseUtil.sendChatBIMd(emitter, "\n> 已生成分析报告：\n\n");

            String reportType = ChatContextHolder.getChatContext().getReportType();
            String reportText = ChatContextHolder.getChatContext().getReport();
            chatAnswer.setReportType(reportType);
            chatAnswer.setReport(reportText);

            if ("markdown".equals(reportType)) {
                SseUtil.sendChatBIMd(emitter, reportText);
            }

            if ("html".equals(reportType)) {
                SseUtil.sendChatBIHtmlReport(emitter, reportText);
            }
        }

        // 4. 保存回答
        chatAnswerService.save(chatAnswer);

        return Tuple.of(answerId, question);
    }

    private boolean isRequestInvalid(SseEmitter emitter, ChatBIRequestDto requestDto) {
        try {
            CommonUtil.validateBean(requestDto);
            return false;
        } catch (ValidationException e) {
            ValidationErrorResponse response = new ValidationErrorResponse();
            response.setErrors(CommonUtil.stream(e.getConstraintViolations()).map(c -> {
                var error = new ValidationErrorResponse.ValidationError();
                error.setField(c.getPropertyPath().toString());
                error.setErrorMsg(c.getMessage());

                return error;
            }).toList());
            Try.run(() -> {
                emitter.send(ChatBIResponseDto.builder()
                        .type(ChatContentType.ERROR)
                        .content(response));
                emitter.complete();
            }).toList();
        }
        return true;
    }
}
