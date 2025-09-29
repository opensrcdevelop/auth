package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.agent.ChartAgent;
import cn.opensrcdevelop.ai.agent.ChatAgent;
import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.client.ChatClientManager;
import cn.opensrcdevelop.ai.chat.tool.ExecutePythonTool;
import cn.opensrcdevelop.ai.constants.MessageConstants;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.dto.ChatBIResponseDto;
import cn.opensrcdevelop.ai.dto.VoteChartRequestDto;
import cn.opensrcdevelop.ai.entity.ChartConf;
import cn.opensrcdevelop.ai.enums.ChatActionType;
import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.ai.model.ChartRecord;
import cn.opensrcdevelop.ai.service.*;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import cn.opensrcdevelop.ai.util.SseUtil;
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
import io.vavr.Tuple3;
import io.vavr.control.Try;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

@Slf4j
@Service
@RequiredArgsConstructor
public class ChatBIServiceImpl implements ChatBIService {

    private static final Long CHAT_TIMEOUT = Duration.ofMinutes(60).toMillis();
    private static final String CHART_RECORD_KEY = "chart_record:%s";
    private static final String ANALYZE_DATA_FILE_NAME = "analyze_data_%s";
    private static final String ANALYZE_DATA_FILE_EXT = ".json";

    private final MessageUtil messageUtil;
    private final ChartConfService chartConfService;
    private final DataSourceManager dataSourceManager;
    private final ChatClientManager chatClientManager;
    private final SqlAgent sqlAgent;
    private final ChartAgent chartAgent;
    private final AnalyzeAgent analyzeAgent;
    private final DataSourceConfService dataSourceConfService;
    private final ChatAgent chatAgent;
    private final ExecutePythonTool executePythonTool;
    private final ChatMessageHistoryService chatMessageHistoryService;
    private final ChatHistoryService chatHistoryService;

    @Resource(name = ExecutorConstants.EXECUTOR_IO_DENSE)
    private Executor executor;

    /**
     * 流式生成图表
     *
     * @param requestDto 请求
     * @return SseEmitter
     */
    @Override
    public SseEmitter streamChatBI(ChatBIRequestDto requestDto) {
        SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);
        SecurityContext securityContext = SecurityContextHolder.getContext();

        if (isRequestInvalid(emitter, requestDto, ChatBIRequestDto.GenerateChart.class)) {
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
                ChatContext.setActionType(ChatActionType.GENERATE_CHART);

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
     * 流式分析数据并生成报告
     *
     * @param requestDto 请求
     * @return SseEmitter
     */
    @Override
    public SseEmitter streamAnalyzeData(ChatBIRequestDto requestDto) {
        SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);
        SecurityContext securityContext = SecurityContextHolder.getContext();

        if (isRequestInvalid(emitter, requestDto, ChatBIRequestDto.AnalyzeData.class)) {
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
                ChatContext.setActionType(ChatActionType.ANALYZE_DATA);

                chatMessageHistoryService.createUserChatMessageHistory("数据分析：" + requestDto.getQuestion());
                processStreamAnalyzeDataRequest(emitter, requestDto, chatId);
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
     * 投票图表
     *
     * @param requestDto 请求
     */
    @Override
    public void voteChart(VoteChartRequestDto requestDto) {
        // 1. 数据库操作
        chartConfService.update(Wrappers.<ChartConf>lambdaUpdate()
                .eq(ChartConf::getChartId, requestDto.getChartId())
                .set(ChartConf::getFeedBack, requestDto.getFeedback() == null ? null : requestDto.getFeedback().name()));
    }

    @SuppressWarnings("unchecked")
    private Tuple2<String, String> processStreamChatBIRequest(SseEmitter emitter, ChatBIRequestDto requestDto, String chatId) throws IOException {
        String dataSourceId = requestDto.getDataSourceId();
        String question = requestDto.getQuestion();

        // 1. 获取 ChatClient
        ChatClient chatClient = chatClientManager.getChatClient(requestDto.getModelProviderId(), requestDto.getModel(), chatId);

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
        SseUtil.sendChatBIText(emitter, "匹配到以下表：\n");
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
        SseUtil.sendChatBIText(emitter, "执行的 SQL：\n");
        SseUtil.sendChatBIMd(emitter, "```sql%n%s%n```".formatted(SqlFormatter.standard().format(executeResult._3)));
        SseUtil.sendChatBIText(emitter, "\n");
        if (!Boolean.TRUE.equals(executeResult._1)) {
            SseUtil.sendChatBIText(emitter, "执行 SQL 失败");
            return Tuple.of(null, rewrittenQuestion);
        }

        List<Map<String, Object>> queryResult = executeResult._2;
        if (CollectionUtils.isEmpty(queryResult)) {
            SseUtil.sendChatBIText(emitter, "查询未返回数据");
            return Tuple.of(null, rewrittenQuestion);
        }

        // 6. 生成图表配置
        SseUtil.sendChatBILoading(emitter, "正在生成图表...");
        Map<String, Object> chartConfResult = chartAgent.generateChart(
                chatClient,
                executeResult._3,
                rewrittenQuestion,
                queryResult
        );
        if (!Boolean.TRUE.equals(chartConfResult.get("success"))) {
            SseUtil.sendChatBITextSegmented(emitter, "无法生成图表，原因：%s".formatted(chartConfResult.get("error")), 500);
            return Tuple.of(null, rewrittenQuestion);
        }

        // 7. 保存图表配置
        String chartId = CommonUtil.getUUIDV7String();
        ChartConf chartConf = new ChartConf();
        chartConf.setModelProviderId(requestDto.getModelProviderId());
        chartConf.setModel(requestDto.getModel());
        chartConf.setChartId(chartId);
        chartConf.setDataSourceId(dataSourceId);
        chartConf.setChatId(chatId);
        chartConf.setQuestionId(requestDto.getQuestionId());
        chartConf.setQuestion(requestDto.getQuestion());
        chartConf.setSql(sql);
        chartConf.setConfig(CommonUtil.serializeObject(chartConfResult.get("config")));
        chartConfService.save(chartConf);

        // 8. 生成图表
        SseUtil.sendChatBIText(emitter, "已生成的图表：");
        var renderResult = ChartRenderer.render(chartConf, queryResult);
        if ("table".equals(renderResult._1)) {
            SseUtil.sendChatBITable(emitter, renderResult._2);
        } else {
            SseUtil.sendChatBIChart(emitter, renderResult._2);
        }

        // 9. 保存临时图表记录
        saveTempChartRecord(chartId, chatId, requestDto.getQuestionId(), rewrittenQuestion, sql, queryResult, (List<Map<String, Object>>) sqlResult.get("columns"));

        return Tuple.of(chartId, rewrittenQuestion);
    }

    @SuppressWarnings("unchecked")
    private void processStreamAnalyzeDataRequest(SseEmitter emitter, ChatBIRequestDto requestDto, String chatId) throws IOException {
        // 1. 获取临时图表记录
        ChartRecord chartRecord = RedisUtil.get(CHART_RECORD_KEY.formatted(requestDto.getChartId()), ChartRecord.class);
        if (Objects.isNull(chartRecord)) {
            SseUtil.sendChatBIText(emitter, "未找到生成的图表，无法进行数据分析。");
            SseUtil.sendChatBIDone(emitter);
            return;
        }

        // 2. 获取 ChatClient
        ChatClient chatClient = chatClientManager.getChatClient(requestDto.getModelProviderId(), requestDto.getModel(), chatId);

        // 3. 分析数据
        File tempDataFile = null;
        String summary;
        String pythonExecutionResult;
        try {
            // 3.1 创建临时数据文件
            tempDataFile = File.createTempFile(ANALYZE_DATA_FILE_NAME.formatted(System.currentTimeMillis()), ANALYZE_DATA_FILE_EXT);
            try (FileWriter writer = new FileWriter(tempDataFile)) {
                writer.write(CommonUtil.serializeObject(chartRecord.getData()));
            }

            // 3.2 生成 Python 代码
            SseUtil.sendChatBILoading(emitter, "正在生成用于分析数据的 Python 代码...");
            Map<String, Object> pythonCodeResult = analyzeAgent.generatePythonCode(chatClient, chartRecord, tempDataFile.getAbsolutePath());
            if (!Boolean.TRUE.equals(pythonCodeResult.get("success"))) {
                SseUtil.sendChatBITextSegmented(emitter, "无法生成用于分析数据的 Python 代码，原因：%s".formatted(pythonCodeResult.get("error")), 500);
                SseUtil.sendChatBIDone(emitter);
                return;
            }

            // 3.3 执行 Python 代码
            SseUtil.sendChatBILoading(emitter, "正在执行用于分析数据的 Python 代码...");
            Tuple3<Boolean, String, String> executeResult = executePythonCodeWithFix(chatClient,
                    emitter,
                    chartRecord,
                    tempDataFile.getAbsolutePath(),
                    (String) pythonCodeResult.get("python_code"),
                    (List<String>) pythonCodeResult.get("packages"),
                    3);
            if (!Boolean.TRUE.equals(executeResult._1)) {
                SseUtil.sendChatBITextSegmented(emitter, "无法执行 Python 代码，原因：%s".formatted(executeResult._2), 500);
                SseUtil.sendChatBIDone(emitter);
                return;
            }

            // 3.4 分析数据
            SseUtil.sendChatBILoading(emitter, "正在分析数据...");
            pythonExecutionResult = executeResult._2;
            Map<String, Object> analyzeResult = analyzeAgent.analyzeData(chatClient, chartRecord, pythonExecutionResult);
            if (!Boolean.TRUE.equals(analyzeResult.get("success"))) {
                SseUtil.sendChatBITextSegmented(emitter, "无法分析数据，原因：%s".formatted(analyzeResult.get("error")), 500);
                SseUtil.sendChatBIDone(emitter);
                return;
            }
            summary = (String) analyzeResult.get("summary");
            SseUtil.sendChatBITextSegmented(emitter, summary, 500);
            SseUtil.sendChatBIText(emitter, "\n");
        } finally {
            if (Objects.nonNull(tempDataFile)) {
                Files.deleteIfExists(tempDataFile.toPath());
            }
        }

        // 4. 判断是否生成报告
        if (!requestDto.isGenerateReport()) {
            // 4.1 分析数据完成，不生成报告
            SseUtil.sendChatBIDone(emitter);
            return;
        }

        // 4.2 生成分析报告
        ChatContext.setActionType(ChatActionType.GENERATE_REPORT);
        SseUtil.sendChatBILoading(emitter, "正在生成分析报告...");
        Map<String, Object> report = analyzeAgent.generateAnalysisReport(chatClient, chartRecord, pythonExecutionResult, summary);
        if (!Boolean.TRUE.equals(report.get("success"))) {
            SseUtil.sendChatBITextSegmented(emitter, "无法生成分析报告，原因：%s".formatted(report.get("error")), 500);
            SseUtil.sendChatBIDone(emitter);
            return;
        }
        SseUtil.sendChatBIText(emitter, "分析报告：\n");
        SseUtil.sendChatBIHtmlReport(emitter, (String) report.get("html"));
        SseUtil.sendChatBIDone(emitter);
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

    @SuppressWarnings("all")
    private Tuple3<Boolean, String, String> executePythonCodeWithFix(ChatClient chatClient,
                                                                     SseEmitter emitter,
                                                                     ChartRecord chartRecord,
                                                                     String dataFilePath,
                                                                     String pythonCode,
                                                                     List<String> packages,
                                                                     int maxAttempts) {
        int attempt = 0;
        String executeOutput = "";
        while (attempt <= maxAttempts) {
            attempt++;
            ExecutePythonTool.Request request = new ExecutePythonTool.Request();
            request.setScript(pythonCode);
            request.setPackages(packages);

            ExecutePythonTool.Response response = executePythonTool.execute(request);
            if (!Boolean.TRUE.equals(response.getSuccess())) {
                log.error("第 {} 次执行 Python 代码失败", attempt);
                SseUtil.sendChatBILoading(emitter, "执行 Python 代码失败，修复并重新执行....");
                Map<String, Object> fixResult = analyzeAgent.fixPythonCode(chatClient, chartRecord, dataFilePath, pythonCode, response.getResult());
                if (!Boolean.TRUE.equals(fixResult.get("success"))) {
                    return Tuple.of(false, response.getResult(), pythonCode);
                }
                pythonCode = (String) fixResult.get("fixed_python_code");
                packages = (List<String>) fixResult.get("packages");
            } else {
                executeOutput = response.getResult();
                break;
            }
        }

        return Tuple.of(true, executeOutput, pythonCode);
    }

    private void saveTempChartRecord(String chartId,
                                     String chatId,
                                     String questionId,
                                     String question,
                                     String sql,
                                     List<Map<String, Object>> data,
                                     List<Map<String, Object>> columns) {
        ChartRecord chartRecord = new ChartRecord();
        chartRecord.setChatId(chatId);
        chartRecord.setQuestionId(questionId);
        chartRecord.setQuestion(question);
        chartRecord.setSql(sql);
        chartRecord.setData(data);
        chartRecord.setColumns(columns);

        RedisUtil.set(CHART_RECORD_KEY.formatted(chartId), chartRecord, 7, TimeUnit.DAYS);
    }

    private boolean isRequestInvalid(SseEmitter emitter, ChatBIRequestDto requestDto, Class<?> validatedClass) {
        try {
            CommonUtil.validateBean(requestDto, validatedClass);
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
