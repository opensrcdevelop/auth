package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.agent.ChatAgent;
import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.client.ChatClientManager;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.dto.VoteAnswerRequestDto;
import cn.opensrcdevelop.ai.entity.ChatAnswer;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.exception.ValidationException;
import cn.opensrcdevelop.common.response.ValidationErrorResponse;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.MessageUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.github.vertical_blank.sqlformatter.SqlFormatter;
import com.zaxxer.hikari.pool.HikariPool;
import io.vavr.Tuple;
import io.vavr.Tuple2;
import io.vavr.Tuple3;
import io.vavr.control.Try;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Executor;

@Slf4j
@Service
@RequiredArgsConstructor
public class ChatBIServiceImpl implements ChatBIService {

    private static final Long CHAT_TIMEOUT = Duration.ofMinutes(60).toMillis();

    private final MessageUtil messageUtil;
    private final ChatAnswerService chatAnswerService;
    private final DataSourceManager dataSourceManager;
    private final ChatClientManager chatClientManager;
    private final SqlAgent sqlAgent;
    private final DataSourceConfService dataSourceConfService;
    private final ChatAgent chatAgent;
    private final ChatMessageHistoryService chatMessageHistoryService;
    private final ChatHistoryService chatHistoryService;

    @Resource(name = ExecutorConstants.EXECUTOR_IO_DENSE)
    private Executor executor;

    /**
     * ChatBI 用户对话
     *
     * @param requestDto 请求
     * @return SseEmitter
     */
    @Override
    public SseEmitter streamChatBI(ChatBIRequestDto requestDto) {
        SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);
        SecurityContext securityContext = SecurityContextHolder.getContext();

        if (isRequestInvalid(emitter, requestDto)) {
            return emitter;
        }

        executor.execute(() -> {
            SecurityContextHolder.setContext(securityContext);
            String chatId = requestDto.getChatId();
            if (StringUtils.isEmpty(chatId)) {
                chatId = CommonUtil.getUUIDV7String();
                chatHistoryService.createChatHistory(chatId, requestDto.getQuestion(), requestDto.getDataSourceId());
            }

            try {
                ChatContext.setChatId(chatId);
                ChatContext.setQuestionId(requestDto.getQuestionId());

                chatMessageHistoryService.createUserChatMessageHistory(requestDto.getQuestion());
                Tuple2<String, String> result = processStreamChatBIRequest(emitter, requestDto, chatId);
                SseUtil.sendChatBIDone(emitter, result._1, result._2);
            } catch (HikariPool.PoolInitializationException ex) {
                SseUtil.sendChatBIError(emitter, messageUtil.getMsg(MessageConstants.AI_DATASOURCE_MSG_1003));
            } catch (Exception ex) {
                log.error(ex.getMessage(), ex);
                SseUtil.sendChatBIError(emitter, messageUtil.getMsg(MessageConstants.AI_CHAT_MSG_1000));
            } finally {
                emitter.complete();
                ChatContext.clearChatContext();
            }
        });

        return emitter;
    }

    /**
     * 投票回答
     *
     * @param requestDto 请求
     */
    @Override
    public void voteAnswer(VoteAnswerRequestDto requestDto) {
        // 1. 数据库操作
        chatAnswerService.update(Wrappers.<ChatAnswer>lambdaUpdate()
                .eq(ChatAnswer::getAnswerId, requestDto.getAnswerId())
                .set(ChatAnswer::getFeedback, requestDto.getFeedback() == null ? null : requestDto.getFeedback().name()));
    }

    @SuppressWarnings("all")
    private Tuple2<String, String> processStreamChatBIRequest(SseEmitter emitter, ChatBIRequestDto requestDto, String chatId) throws IOException {
        String dataSourceId = requestDto.getDataSourceId();
        String question = requestDto.getQuestion();

        // 1. 获取 ChatClient
        ChatClient chatClient = chatClientManager.getChatClient(requestDto.getModelProviderId(), requestDto.getModel(), chatId);
        ChatContext.setChatClient(chatClient);

        // 2. 重写用户提问
        String rewrittenQuestion = question;
        Map<String, Object> rewriteUserQuestionResult = chatAgent.rewriteUserQuestion(chatClient, question);
        if (Boolean.TRUE.equals(rewriteUserQuestionResult.get("success"))) {
            rewrittenQuestion = (String) rewriteUserQuestionResult.get("rewritten_question");
        }
        chatHistoryService.updateChatHistory(chatId, rewrittenQuestion);

        // 3. 根据用户问题获取关联的表信息
        SseUtil.sendChatBILoading(emitter, "正在匹配相关表信息...");
        // 3.1 检查数据源是否已同步
        if (Boolean.FALSE.equals(dataSourceConfService.isSynced(dataSourceId))) {
            SseUtil.sendChatBIText(emitter, "数据源未同步，请先执行同步表操作。");
            return Tuple.of(null, rewrittenQuestion);
        }
        // 3.2 获取相关表信息
        Map<String, Object> tableResult = sqlAgent.getRelevantTables(chatClient, rewrittenQuestion, dataSourceId);
        if (!Boolean.TRUE.equals(tableResult.get("success"))) {
            SseUtil.sendChatBITextSegmented(emitter, "无法获取相关表信息，原因：%s".formatted(tableResult.get("error")), 500);
            return Tuple.of(null, rewrittenQuestion);
        }
        List<Map<String, Object>> relevantTables = (List<Map<String, Object>>) tableResult.get("tables");
        SseUtil.sendChatBIMd(emitter, "> 匹配到以下表：\n\n");
        for (Map<String, Object> table : relevantTables) {
            SseUtil.sendChatBIMd(emitter, "`%s` ".formatted(table.get("table_name")));
        }

        // 4. 生成 SQL
        SseUtil.sendChatBILoading(emitter, "正在生成 SQL...");
        Map<String, Object> sqlResult = sqlAgent.generateSql(chatClient, rewrittenQuestion, relevantTables, dataSourceId);
        if (!Boolean.TRUE.equals(sqlResult.get("success"))) {
            SseUtil.sendChatBITextSegmented(emitter, "无法生成 SQL，原因：%s".formatted(sqlResult.get("error")), 500);
            return Tuple.of(null, rewrittenQuestion);
        }
        String sql = (String) sqlResult.get("sql");

        // 5. 执行 SQL
        SseUtil.sendChatBILoading(emitter, "正在执行 SQL...");
        var executeResult = executeSqlWithFix(chatClient, emitter, sql, dataSourceId, relevantTables, 5);
        SseUtil.sendChatBIMd(emitter, "\n> 执行的 SQL：\n\n");
        SseUtil.sendChatBIMd(emitter, "```sql%n%s%n```".formatted(SqlFormatter.standard().format(executeResult._3)));
        if (!Boolean.TRUE.equals(executeResult._1)) {
            SseUtil.sendChatBIText(emitter, "执行 SQL 失败");
            return Tuple.of(null, rewrittenQuestion);
        }

        List<Map<String, Object>> queryResult = executeResult._2;
        if (CollectionUtils.isEmpty(queryResult)) {
            SseUtil.sendChatBIText(emitter, "查询未返回数据");
            return Tuple.of(null, rewrittenQuestion);
        }

        ChatContext.setSql(executeResult._3);
        ChatContext.setQueryData(queryResult);
        ChatContext.setQueryColumns((List<Map<String, Object>>) sqlResult.get("columns"));

        // 6. 回答问题
        SseUtil.sendChatBILoading(emitter, "正在回答问题...");
        Map<String, Object> answer = chatAgent.answerQuestion(
                chatClient,
                rewrittenQuestion,
                ChatContext.getQueryData(),
                ChatContext.getQueryColumns()
        );
        if (MapUtils.isEmpty(answer)) {
            SseUtil.sendChatBIText(emitter, "抱歉无法回答您的提问，请稍后重试。");
            return Tuple.of(null, rewrittenQuestion);
        }

        SseUtil.sendChatBIMd(emitter, "\n> 回答如下：\n\n");

        String answerId = CommonUtil.getUUIDV7String();
        ChatAnswer chatAnswer = new ChatAnswer();
        chatAnswer.setAnswerId(answerId);
        chatAnswer.setModelProviderId(requestDto.getModelProviderId());
        chatAnswer.setModel(requestDto.getModel());
        chatAnswer.setDataSourceId(dataSourceId);
        chatAnswer.setChatId(chatId);
        chatAnswer.setQuestionId(requestDto.getQuestionId());
        chatAnswer.setQuestion(requestDto.getQuestion());
        chatAnswer.setSql(sql);

        // 6.1 直接回答
        if (answer.containsKey("answer")) {
            String answerText = (String) answer.get("answer");
            chatAnswer.setAnswer(answerText);
            SseUtil.sendChatBITextSegmented(emitter, answerText, 500);
        }

        // 6.2 图表
        if (answer.containsKey("chart") && Objects.nonNull(answer.get("chart"))) {

            // 6.2.1 生成图表
            Map<String, Object> chartConfig = (Map<String, Object>) answer.get("chart");
            chatAnswer.setChartConfig(CommonUtil.serializeObject(chartConfig));
            var renderResult = ChartRenderer.render(chartConfig, queryResult);
            if ("table".equals(renderResult._1)) {
                SseUtil.sendChatBITable(emitter, renderResult._2);
            } else {
                SseUtil.sendChatBIChart(emitter, renderResult._2);
            }
        }

        // 6.3 报告
        if (answer.containsKey("report") && answer.containsKey("report_type") && Objects.nonNull(answer.get("report_type"))) {

            SseUtil.sendChatBIMd(emitter, "\n> 已生成分析报告：\n\n");

            String reportType = (String) answer.get("report_type");
            String reportText = (String) answer.get("report");
            chatAnswer.setReportType(reportType);
            chatAnswer.setReport(reportText);

            if ("markdown".equals(reportType)) {
                SseUtil.sendChatBIMd(emitter, reportText);
            }

            if ("html".equals(reportType)) {
                SseUtil.sendChatBIHtmlReport(emitter, reportText);
            }
        }

        // 6.4 保存回答
        chatAnswerService.save(chatAnswer);

        return Tuple.of(answerId, rewrittenQuestion);
    }

    @SuppressWarnings("all")
    private Tuple3<Boolean, List<Map<String, Object>>, String> executeSqlWithFix(ChatClient chatClient,
                                                                                 SseEmitter emitter,
                                                                                 String sql,
                                                                                 String dataSourceId,
                                                                                 List<Map<String, Object>> relevantTables,
                                                                                 int maxAttempts) throws IOException {
        JdbcTemplate jdbcTemplate = dataSourceManager.getJdbcTemplate(dataSourceId);
        int attempt = 0;
        List<Map<String, Object>> queryResult = new ArrayList<>();

        while (attempt <= maxAttempts) {
            attempt++;
            try {
                queryResult = jdbcTemplate.queryForList(sql);
                break;
            } catch (Exception ex) {
                SseUtil.sendChatBILoading(emitter, "执行 SQL 失败，修复并重新执行....");
                log.error("第 {} 次执行 SQL 失败", attempt);
                log.error(ex.getCause().getMessage(), ex);
                if (attempt > maxAttempts) {
                    return Tuple.of(false, queryResult, sql);
                }
                Map<String, Object> sqlResult = sqlAgent.fixSql(chatClient, sql, ex.getCause().getMessage(), relevantTables, dataSourceId);
                if (!Boolean.TRUE.equals(sqlResult.get("success"))) {
                    return Tuple.of(false, queryResult, sql);
                }
                sql = (String) sqlResult.get("sql");
            }
        }

        return Tuple.of(true, queryResult, sql);
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
