package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.agent.ThinkAnswerAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.client.ChatClientManager;
import cn.opensrcdevelop.ai.chat.tool.impl.RewriteUserQuestionTool;
import cn.opensrcdevelop.ai.component.HeartbeatManager;
import cn.opensrcdevelop.ai.component.QueryResultTempFileManager;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.constants.RedisTopicConstants;
import cn.opensrcdevelop.ai.constants.SystemSettingConstants;
import cn.opensrcdevelop.ai.dto.*;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.enums.Feedback;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.compare.CompareObj;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.enums.ResourceType;
import cn.opensrcdevelop.auth.audit.enums.SysOperationType;
import cn.opensrcdevelop.auth.audit.enums.UserOperationType;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.exception.ValidationException;
import cn.opensrcdevelop.common.response.ValidationErrorResponse;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.MessageUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.vertical_blank.sqlformatter.SqlFormatter;
import com.zaxxer.hikari.pool.HikariPool;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.control.Try;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.Executor;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.atomic.AtomicBoolean;

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
    private final SampleSqlService sampleSqlService;
    private final RewriteUserQuestionTool rewriteUserQuestionTool;
    private final HeartbeatManager heartbeatManager;
    private final QueryResultTempFileManager queryResultTempFileManager;
    private final SystemSettingService systemSettingService;

    @Resource(name = ExecutorConstants.EXECUTOR_IO_DENSE)
    private Executor executor;

    @Value("${ai.chat.default-max-think-steps:30}")
    private Integer defaultMaxThinkSteps;

    @Value("${ai.chat.default-max-consecutive-tool-calls:3}")
    private Integer defaultMaxConsecutiveToolCalls;

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

        ScheduledFuture<?> heartbeatFuture = heartbeatManager.startHeartbeat(emitter);

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
            chatContext.setId(UUID.randomUUID().toString());

            // 注册 SSE 回调
            emitter.onCompletion(() -> {
                log.info("ChatBI 对话（{}）中断/结束", finalChatId);
                interruptFlag.set(true);
                heartbeatManager.stopHeartbeat(heartbeatFuture);
                cleanupTempFiles(chatContext);
            });

            emitter.onTimeout(() -> {
                log.info("ChatBI 对话（{}）超时", finalChatId);
                interruptFlag.set(true);
                heartbeatManager.stopHeartbeat(heartbeatFuture);
                cleanupTempFiles(chatContext);
            });

            emitter.onError(e -> {
                log.info("ChatBI 对话（{}）异常: {}", finalChatId, e.getMessage());
                interruptFlag.set(true);
                heartbeatManager.stopHeartbeat(heartbeatFuture);
                cleanupTempFiles(chatContext);
            });

            try {
                chatContext.setEmitter(emitter);
                chatContext.setChatId(finalChatId);
                chatContext.setQuestionId(requestDto.getQuestionId());
                chatContext.setDataSourceId(requestDto.getDataSourceId());
                chatContext.setQuestion(requestDto.getQuestion());
                chatContext.setRawQuestion(requestDto.getQuestion());
                ChatContextHolder.setChatContext(chatContext);

                log.info("ChatBI 对话（{}）开始 -  ChatContextID: {}, ModelProviderID: {}, Model: {}",
                        finalChatId,
                        chatContext.getId(),
                        requestDto.getModelProviderId(),
                        requestDto.getModel());

                chatMessageHistoryService.createUserChatMessageHistory(requestDto.getQuestion());

                Tuple2<String, String> result = processStreamChatBIRequest(emitter, interruptFlag, requestDto,
                        finalChatId);

                if (!interruptFlag.get()) {
                    SseUtil.sendChatBIDone(emitter, result._1);
                } else {
                    chatMessageHistoryService.createChatMessageHistory("回答已取消", ChatContentType.LOADING);
                    SseUtil.sendChatBIDone(emitter);
                }
            } catch (HikariPool.PoolInitializationException ex) {
                SseUtil.sendChatBIError(emitter, messageUtil.getMsg(MessageConstants.AI_DATASOURCE_MSG_1003));
                SseUtil.sendChatBIDone(emitter);
            } catch (Exception ex) {
                log.error(ex.getMessage(), ex);
                SseUtil.sendChatBIError(emitter, messageUtil.getMsg(MessageConstants.AI_CHAT_MSG_1000));
                SseUtil.sendChatBIDone(emitter);
            } finally {
                cleanupTempFiles(chatContext);
                emitter.complete();
                ChatContextHolder.removeChatContext(finalChatId);
            }
        });

        return emitter;
    }

    /**
     * 清理会话的所有临时文件
     */
    private void cleanupTempFiles(ChatContext chatContext) {
        if (chatContext == null) {
            return;
        }
        List<String> paths = chatContext.getQueryResultFilePaths();
        if (paths != null && !paths.isEmpty()) {
            for (String path : paths) {
                try {
                    queryResultTempFileManager.deleteTempFile(path);
                } catch (Exception e) {
                    log.warn("清理临时文件失败: {}", path, e);
                }
            }
            chatContext.clearQueryResultFilePaths();
        }
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

        // 2. 同步向量库
        try {
            if (requestDto.getFeedback() == Feedback.LIKE) {
                sampleSqlService.addToVectorStore(requestDto.getAnswerId());
            } else if (requestDto.getFeedback() == Feedback.DISLIKE) {
                sampleSqlService.removeFromVectorStore(requestDto.getAnswerId());
            }
        } catch (Exception e) {
            log.error("同步向量库失败，不影响投票结果", e);
        }
    }

    /**
     * 回答 AI 对用户的提问
     *
     * @param requestDto
     *            回答 AI 对用户的提问请求
     */
    @Override
    public void answerAskUserQuestion(UserAnswerRequestDto requestDto) {
        RedisUtil.publishMessage(RedisTopicConstants.getTopic(requestDto.getChatId()), requestDto);
    }

    /**
     * 获取对话配置
     *
     * @return 对话配置
     */
    @Override
    public ChatConfigDto getChatConfig() {
        return systemSettingService.getSystemSetting(SystemSettingConstants.CHATBI_CHAT_CONFIG, ChatConfigDto.class);
    }

    /**
     * 更新对话配置
     *
     * @param configDto
     *            对话配置
     */
    @Audit(type = AuditType.SYS_OPERATION, resource = ResourceType.CHAT_BI, sysOperation = SysOperationType.UPDATE, success = "更新了 ChatBI 对话配置", fail = "更新 ChatBI 对话配置失败")
    @Override
    public void updateChatConfig(ChatConfigDto configDto) {
        // 审计比较对象
        var compareObjBuilder = CompareObj.builder();

        ChatConfigDto rawChatConfigDto = systemSettingService
                .getSystemSetting(SystemSettingConstants.CHATBI_CHAT_CONFIG, ChatConfigDto.class);

        compareObjBuilder.before(rawChatConfigDto);
        compareObjBuilder.after(configDto);

        systemSettingService.saveSystemSetting(SystemSettingConstants.CHATBI_CHAT_CONFIG, configDto);

        AuditContext.addCompareObj(compareObjBuilder.build());
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
        rewriteUserQuestion(chatId, question);
        String finalQuestion = ChatContextHolder.getChatContext().getQuestion();

        // 2.2 获取示例 SQL（用户反馈为 LIKE 的历史问题-SQL）
        List<Map<String, String>> sampleSqls = getSampleSqls(dataSourceId, finalQuestion);

        // 2.3 获取会话历史用户消息
        List<String> historicalQuestions = chatMessageHistoryService.getUserHistoryQuestions(
                ChatContextHolder.getChatContext().getChatId());
        if (CollectionUtils.isNotEmpty(historicalQuestions)) {
            historicalQuestions = new ArrayList<>(historicalQuestions);
            historicalQuestions.removeLast();
            historicalQuestions = CommonUtil.stream(historicalQuestions).distinct().toList();
        }

        // 2.4 获取对话配置
        ChatConfigDto chatConfig = null;
        try {
            chatConfig = systemSettingService.getSystemSetting(
                    SystemSettingConstants.CHATBI_CHAT_CONFIG, ChatConfigDto.class);
        } catch (Exception e) {
            log.error("获取 ChatBI 对话配置失败", e);
        }
        if (Objects.isNull(chatConfig)) {
            chatConfig = new ChatConfigDto();
        }
        ChatContextHolder.getChatContext().setChatConfig(chatConfig);

        int maxSteps = Objects.nonNull(chatConfig.getMaxThinkSteps()) && chatConfig.getMaxThinkSteps() > 0
                ? chatConfig.getMaxThinkSteps()
                : defaultMaxThinkSteps;
        int maxConsecutiveToolCalls = Objects.nonNull(chatConfig.getMaxConsecutiveToolCalls())
                && chatConfig.getMaxConsecutiveToolCalls() >= 2
                        ? chatConfig.getMaxConsecutiveToolCalls()
                        : defaultMaxConsecutiveToolCalls;

        // 3. 回答问题
        SseUtil.sendChatBILoading(emitter, "正在回答问题...");
        Map<String, Object> answer = thinkAnswerAgent.thinkAnswer(
                emitter,
                interruptFlag,
                chatClient,
                sampleSqls,
                historicalQuestions,
                maxSteps,
                Boolean.TRUE.equals(requestDto.getShowThinking()),
                maxConsecutiveToolCalls);

        if (interruptFlag.get()) {
            return Tuple.of(null, question);
        }

        // 检测是否需要等待用户回答
        if (answer != null && Boolean.TRUE.equals(answer.get("isWaitingForUser"))) {
            // ask_user tool 已被调用，等待用户回答，不保存答案
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
        chatAnswer.setInputTokens(ChatContextHolder.getChatContext().getInputTokens().get());
        chatAnswer.setOutputTokens(ChatContextHolder.getChatContext().getOutputTokens().get());

        // 3.1 发送数据查询结果
        String sql = ChatContextHolder.getChatContext().getSql();
        if (StringUtils.isNotBlank(sql)) {
            SseUtil.sendChatBIMd(emitter, "\n> 数据查询：\n\n");

            List<Map<String, Object>> queryData = ChatContextHolder.getChatContext().getQueryData();
            List<Map<String, Object>> queryColumns = ChatContextHolder.getChatContext().getQueryColumns();
            Map<String, Object> tableMessage = new HashMap<>();
            tableMessage.put("sql", SqlFormatter.standard().format(sql));
            var tableConfig = ChartRenderer.buildArcoTableConfig(queryData, queryColumns);
            tableMessage.putAll(tableConfig);
            SseUtil.sendChatBITable(emitter, tableMessage);
        }

        // 3.2 直接回答
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
            SseUtil.sendChatBITextSegmented(emitter, answerText, ChatContentType.MARKDOWN, 30);
        }

        // 3.3 图表
        Map<String, Object> chartConfig = ChatContextHolder.getChatContext().getChartConfig();
        if (MapUtils.isNotEmpty(chartConfig)) {
            chatAnswer.setChartConfig(CommonUtil.serializeObject(chartConfig));
            var renderResult = ChartRenderer.render(chartConfig, ChatContextHolder.getChatContext().getQueryData());
            SseUtil.sendChatBIChart(emitter, renderResult._2);
        }

        // 3.3 报告
        String reportType = ChatContextHolder.getChatContext().getReportType();
        String reportText = ChatContextHolder.getChatContext().getReport();
        if (StringUtils.isNotBlank(reportType) && StringUtils.isNotBlank(reportText)) {
            chatAnswer.setReportType(reportType);
            chatAnswer.setReport(reportText);

            SseUtil.sendChatBIMd(emitter, "\n> 已生成分析报告：\n\n");

            if ("markdown".equals(reportType)) {
                SseUtil.sendChatBIMdReport(emitter, reportText);
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
     */
    private void rewriteUserQuestion(String chatId, String rawQuestion) {
        // 检查 ChatContext 中的 question 是否已被重写
        String currentQuestion = ChatContextHolder.getChatContext().getQuestion();
        if (StringUtils.isNotBlank(currentQuestion) && !currentQuestion.equals(rawQuestion)) {
            // 已经被重写过了，直接返回
            return;
        }

        // 执行重写
        try {
            RewriteUserQuestionTool.Request request = new RewriteUserQuestionTool.Request();
            request.setInstruction(null);
            RewriteUserQuestionTool.Response response = rewriteUserQuestionTool.execute(request);

            if (Boolean.TRUE.equals(response.getSuccess()) && StringUtils.isNotBlank(response.getRewrittenQuestion())) {
                String rewrittenQuestion = response.getRewrittenQuestion();
                // 更新 ChatHistory 的标题
                chatHistoryService.updateChatHistory(chatId, rewrittenQuestion);
                log.info("会话 {} 重写问题: {} -> {}", chatId, rawQuestion, rewrittenQuestion);
            } else {
                log.warn("会话 {} 重写问题失败，使用原始问题", chatId);
            }
        } catch (Exception e) {
            log.error("会话 {} 重写问题时发生异常", chatId, e);
        }
    }

    /**
     * 获取与当前问题相关的示例 SQL
     *
     * @param dataSourceId
     *            数据源ID
     * @param currentQuestion
     *            当前问题
     * @return 相关的问题-SQL 对列表
     */
    private List<Map<String, String>> getSampleSqls(String dataSourceId, String currentQuestion) {
        try {
            List<SampleSqlDto> sampleSqls = sampleSqlService.search(dataSourceId, currentQuestion, null);

            List<Map<String, String>> result = new ArrayList<>();
            for (SampleSqlDto dto : sampleSqls) {
                result.add(Map.of("question", dto.getQuestion(), "sql", dto.getSql()));
            }

            log.info("向量检索返回 {} 条相关示例 SQL", result.size());
            return result;
        } catch (Exception e) {
            log.error("获取示例 SQL 失败", e);
            return new ArrayList<>();
        }
    }
}
