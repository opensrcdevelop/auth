package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.agent.ChatAgent;
import cn.opensrcdevelop.ai.agent.ThinkAnswerAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.client.ChatClientManager;
import cn.opensrcdevelop.ai.chat.tool.impl.RewriteUserQuestionTool;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.dto.SampleSqlDto;
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
import java.util.ArrayList;
import java.util.List;
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
    private final ChatAgent chatAgent;
    private final ChatMessageHistoryService chatMessageHistoryService;
    private final ChatHistoryService chatHistoryService;
    private final RewriteUserQuestionTool rewriteUserQuestionTool;

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

        // 2.1 第一步：重写用户问题
        rewriteUserQuestion(chatId, question, chatClient);
        String finalQuestion = ChatContextHolder.getChatContext().getQuestion();

        // 2.2 获取示例 SQL（用户反馈为 LIKE 的历史问题-SQL）
        List<Map<String, String>> sampleSqls = getSampleSqls(dataSourceId, finalQuestion, chatClient);

        // 3. 回答问题
        SseUtil.sendChatBILoading(emitter, "正在回答问题...");
        Map<String, Object> answer = thinkAnswerAgent.thinkAnswer(
                emitter,
                interruptFlag,
                chatClient,
                finalQuestion,
                sampleSqls,
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
        chatAnswer.setQuestion(finalQuestion);
        chatAnswer.setSql(ChatContextHolder.getChatContext().getSql());
        chatAnswer.setReqTokens(ChatContextHolder.getChatContext().getReqTokens().get());
        chatAnswer.setRepTokens(ChatContextHolder.getChatContext().getRepTokens().get());

        // 3.1 直接回答
        String answerText = null;
        if (answer.containsKey("final_answer")) {
            Object finalAnswerValue = answer.get("final_answer");
            if (finalAnswerValue instanceof String) {
                answerText = (String) finalAnswerValue;
            } else if (finalAnswerValue instanceof Map) {
                // 如果 final_answer 被解析为 Map，尝试获取 content 或 text 字段
                Map<String, Object> finalAnswerMap = (Map<String, Object>) finalAnswerValue;
                if (finalAnswerMap.containsKey("content")) {
                    Object content = finalAnswerMap.get("content");
                    answerText = content instanceof String ? (String) content : content.toString();
                } else if (finalAnswerMap.containsKey("text")) {
                    Object text = finalAnswerMap.get("text");
                    answerText = text instanceof String ? (String) text : text.toString();
                } else {
                    answerText = finalAnswerMap.toString();
                }
            }
        } else if (answer.containsKey("content")) {
            // final_answer 已被解析，content 在外层
            Object content = answer.get("content");
            answerText = content instanceof String ? (String) content : content.toString();
        } else if (answer.containsKey("text")) {
            // final_answer 已被解析，text 在外层
            Object text = answer.get("text");
            answerText = text instanceof String ? (String) text : text.toString();
        }

        if (answerText != null) {
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

    /**
     * 重写用户问题，作为对话的第一步执行
     *
     * @param chatId
     *            对话ID
     * @param rawQuestion
     *            原始问题
     * @param chatClient
     *            ChatClient
     * @return 重写后的问题，如果重写失败则返回原始问题
     */
    private String rewriteUserQuestion(String chatId, String rawQuestion, ChatClient chatClient) {
        // 检查 ChatContext 中的 question 是否已被重写
        String currentQuestion = ChatContextHolder.getChatContext().getQuestion();
        if (StringUtils.isNotBlank(currentQuestion) && !currentQuestion.equals(rawQuestion)) {
            // 已经被重写过了，直接返回
            return currentQuestion;
        }

        // 执行重写
        try {
            RewriteUserQuestionTool.Request request = new RewriteUserQuestionTool.Request();
            request.setInstruction(null);
            RewriteUserQuestionTool.Response response = rewriteUserQuestionTool.execute(request);

            if (Boolean.TRUE.equals(response.getSuccess()) && StringUtils.isNotBlank(response.getRewrittenQuestion())) {
                String rewrittenQuestion = response.getRewrittenQuestion();
                // 更新 ChatContext 中的 question
                ChatContextHolder.getChatContext().setQuestion(rewrittenQuestion);
                // 更新 ChatHistory 的标题
                chatHistoryService.updateChatHistory(chatId, rewrittenQuestion);
                log.info("会话 {} 重写问题: {} -> {}", chatId, rawQuestion, rewrittenQuestion);
                return rewrittenQuestion;
            } else {
                log.warn("会话 {} 重写问题失败，使用原始问题", chatId);
                return rawQuestion;
            }
        } catch (Exception e) {
            log.error("会话 {} 重写问题时发生异常", chatId, e);
            return rawQuestion;
        }
    }

    /**
     * 获取与当前问题相关的示例 SQL
     *
     * @param dataSourceId
     *            数据源ID
     * @param currentQuestion
     *            当前问题
     * @param chatClient
     *            ChatClient
     * @return 相关的问题-SQL 对列表
     */
    private List<Map<String, String>> getSampleSqls(String dataSourceId, String currentQuestion,
            ChatClient chatClient) {
        try {
            // 1. 获取历史 LIKE 回答（包含 answerId 和 question）
            List<ChatAnswer> historicalAnswers = chatAnswerService.list(Wrappers.<ChatAnswer>lambdaQuery()
                    .select(ChatAnswer::getAnswerId, ChatAnswer::getQuestion)
                    .eq(ChatAnswer::getDataSourceId, dataSourceId)
                    .eq(ChatAnswer::getFeedback, "LIKE")
                    .isNotNull(ChatAnswer::getSql)
                    .ne(ChatAnswer::getSql, "")
                    .last("LIMIT 500"));
            log.info("获取到 {} 条历史 LIKE 回答", historicalAnswers.size());
            if (historicalAnswers.isEmpty()) {
                return new ArrayList<>();
            }

            // 2. 转换为 Map 列表（只包含 answerId 和 question）
            List<Map<String, String>> answerMaps = new ArrayList<>();
            for (ChatAnswer answer : historicalAnswers) {
                answerMaps.add(Map.of("answerId", answer.getAnswerId(), "question", answer.getQuestion()));
            }

            // 3. 让 Agent 判断相关性，返回相关的 answerId 列表
            Map<String, Object> filterResult = chatAgent.filterRelatedHistoricalAnswers(
                    chatClient, currentQuestion, answerMaps, 5);
            List<String> relatedAnswerIds = new ArrayList<>();
            if (filterResult.containsKey("related_answer_ids")
                    && filterResult.get("related_answer_ids") instanceof List) {
                Object ids = filterResult.get("related_answer_ids");
                for (Object id : (List<?>) ids) {
                    if (id instanceof String) {
                        relatedAnswerIds.add((String) id);
                    }
                }
            }
            log.info("Agent 返回 {} 个相关 answerId: {}", relatedAnswerIds.size(), relatedAnswerIds);

            if (relatedAnswerIds.isEmpty()) {
                return new ArrayList<>();
            }

            // 4. 根据 answerId 查询对应的 SQL
            List<SampleSqlDto> sampleSqls = chatAnswerService.getSqlsByAnswerIds(relatedAnswerIds);
            log.info("根据 answerId 查询到 {} 条示例 SQL", sampleSqls.size());

            // 5. 转换为 Map 列表返回
            List<Map<String, String>> result = new ArrayList<>();
            for (SampleSqlDto dto : sampleSqls) {
                result.add(Map.of("question", dto.getQuestion(), "sql", dto.getSql()));
            }

            return result;
        } catch (Exception e) {
            log.error("获取示例 SQL 失败", e);
            return new ArrayList<>();
        }
    }
}
