package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.agent.ChartAgent;
import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatClientManager;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.entity.ChartConf;
import cn.opensrcdevelop.ai.enums.ChatActionType;
import cn.opensrcdevelop.ai.model.ChartRecord;
import cn.opensrcdevelop.ai.service.ChartConfService;
import cn.opensrcdevelop.ai.service.ChatBIService;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import com.github.vertical_blank.sqlformatter.SqlFormatter;
import io.vavr.Tuple;
import io.vavr.Tuple3;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
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
    private static final String CHART_RECORD_KEY = "chat_record:%s";

    private final ChartConfService chartConfService;
    private final DataSourceManager dataSourceManager;
    private final ChatClientManager chatClientManager;
    private final SqlAgent sqlAgent;
    private final ChartAgent chartAgent;
    private final AnalyzeAgent analyzeAgent;

    @Resource(name = ExecutorConstants.EXECUTOR_IO_DENSE)
    private Executor executor;

    @Override
    public SseEmitter streamChatBI(ChatBIRequestDto requestDto) {
        SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);

        executor.execute(() -> {
            String chatId = Objects.isNull(requestDto.getChatId()) ? CommonUtil.getUUIDV7String() : requestDto.getChatId();
            try {
                ChatContext.setChatId(chatId);
                ChatContext.setQuestionId(requestDto.getQuestionId());
                ChatContext.setActionType(ChatActionType.GENERATE_CHART);
                String chartId = processStreamRequest(requestDto, emitter, chatId);
                SseUtil.sendChatBIDone(emitter, chartId);
            } catch (Exception ex) {
                emitter.completeWithError(ex);
            } finally {
                emitter.complete();
                ChatContext.clearChatContext();
            }
        });

        return emitter;
    }

    @Override
    @SuppressWarnings("unchecked")
    public SseEmitter streamAnalyzeData(ChatBIRequestDto requestDto) {
        SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);
        String chatId = requestDto.getChatId();
        String questionId = requestDto.getQuestionId();

        executor.execute(() -> {
            try {
                ChatContext.setChatId(chatId);
                ChatContext.setQuestionId(questionId);
                ChatContext.setActionType(ChatActionType.ANALYZE_DATA);

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
                SseUtil.sendChatBILoading(emitter, "正在分析数据...");
                Map<String, Object> analyzeResult = analyzeAgent.analyzeData(chatClient, chartRecord);
                if (!Boolean.TRUE.equals(analyzeResult.get("success"))) {
                    SseUtil.sendChatBITextSegmented(emitter, "无法分析数据，原因：%s".formatted(analyzeResult.get("error")), 500);
                    return;
                }
                List<String> insights = (List<String>) analyzeResult.get("insights");
                for (String insight : insights) {
                    SseUtil.sendChatBIMd(emitter, "- **%s**".formatted(insight));
                    SseUtil.sendChatBIText(emitter, "\n");
                }
                SseUtil.sendChatBIDone(emitter);
            } catch (Exception ex) {
                emitter.completeWithError(ex);
            } finally {
                emitter.complete();
                ChatContext.clearChatContext();
            }
        });
        return emitter;
    }

    @SuppressWarnings("unchecked")
    private String processStreamRequest(ChatBIRequestDto requestDto, SseEmitter emitter, String chatId) throws IOException {
        String dataSourceId = requestDto.getDataSourceId();

        // 1. 获取 ChatClient
        ChatClient chatClient = chatClientManager.getChatClient(requestDto.getModelProviderId(), requestDto.getModel(), chatId);

        // 2. 根据用户问题获取关联的表信息
        SseUtil.sendChatBILoading(emitter, "正在匹配相关表信息...");
        Map<String, Object> tableResult = sqlAgent.getRelevantTables(chatClient, requestDto.getQuestion(), dataSourceId);
        if (!Boolean.TRUE.equals(tableResult.get("success"))) {
            SseUtil.sendChatBITextSegmented(emitter, "无法获取相关表信息，原因：%s".formatted(tableResult.get("error")), 500);
            return null;
        }
        List<Map<String, Object>> relevantTables = (List<Map<String, Object>>) tableResult.get("tables");
        SseUtil.sendChatBIText(emitter, "匹配到以下表：\n");
        for (Map<String, Object> table : relevantTables) {
            SseUtil.sendChatBIMd(emitter, "`%s` ".formatted(table.get("table_name")));
        }

        // 3. 生成 SQL
        SseUtil.sendChatBILoading(emitter, "正在生成 SQL...");
        Map<String, Object> sqlResult = sqlAgent.generateSql(chatClient, requestDto.getQuestion(), relevantTables, dataSourceId);
        if (!Boolean.TRUE.equals(sqlResult.get("success"))) {
            SseUtil.sendChatBITextSegmented(emitter, "无法生成 SQL，原因：%s".formatted(sqlResult.get("error")), 500);
            return null;
        }
        String sql = (String) sqlResult.get("sql");

        // 4. 执行 SQL
        SseUtil.sendChatBILoading(emitter, "正在执行 SQL...");
        var executeResult = executeSqlWithRepair(chatClient, emitter, sql, dataSourceId, relevantTables, 5);
        SseUtil.sendChatBIText(emitter, "执行的 SQL：\n");
        SseUtil.sendChatBIMd(emitter, "```sql%n%s%n```".formatted(SqlFormatter.standard().format(executeResult._3)));
        SseUtil.sendChatBIText(emitter, "\n");
        if (!Boolean.TRUE.equals(executeResult._1)) {
            SseUtil.sendChatBIText(emitter, "执行 SQL 失败");
            return null;
        }

        List<Map<String, Object>> queryResult = executeResult._2;
        if (CollectionUtils.isEmpty(queryResult)) {
            SseUtil.sendChatBIText(emitter, "查询未返回数据");
            return null;
        }

        // 5. 生成图表配置
        SseUtil.sendChatBILoading(emitter, "正在生成图表...");
        Map<String, Object> chartConfResult = chartAgent.generateChart(
                chatClient,
                executeResult._3,
                requestDto.getQuestion(),
                queryResult
        );
        if (!Boolean.TRUE.equals(chartConfResult.get("success"))) {
            SseUtil.sendChatBITextSegmented(emitter, "无法生成图表，原因：%s".formatted(chartConfResult.get("error")), 500);
            return null;
        }

        // 6. 保存图表配置
        String chartId = CommonUtil.getUUIDV7String();
        ChartConf chartConf = new ChartConf();
        chartConf.setChartId(chartId);
        chartConf.setDataSourceId(dataSourceId);
        chartConf.setChatId(chatId);
        chartConf.setQuestionId(requestDto.getQuestionId());
        chartConf.setQuestion(requestDto.getQuestion());
        chartConf.setSql(sql);
        chartConf.setConfig(CommonUtil.serializeObject(chartConfResult.get("config")));
        chartConfService.save(chartConf);

        // 7. 生成图表
        SseUtil.sendChatBIText(emitter, "已生成的图表：");
        var renderResult = ChartRenderer.render(chartConf, queryResult);
        if ("table".equals(renderResult._1)) {
            SseUtil.sendChatBITable(emitter, renderResult._2);
        } else {
            SseUtil.sendChatBIChart(emitter, renderResult._2);
        }

        // 8. 保存临时图表记录
        saveTempChartRecord(chartId, chatId, requestDto.getQuestionId(), requestDto.getQuestion(), sql, queryResult, (List<Map<String, Object>>) sqlResult.get("columns"));

        return chartId;
    }

    @SuppressWarnings("all")
    private Tuple3<Boolean, List<Map<String, Object>>, String> executeSqlWithRepair(ChatClient chatClient,
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
                Map<String, Object> sqlResult = sqlAgent.repairSql(chatClient, sql, ex.getCause().getMessage(), relevantTables, dataSourceId);
                if (!Boolean.TRUE.equals(sqlResult.get("success"))) {
                    return Tuple.of(false, queryResult, sql);
                }
                sql = (String) sqlResult.get("sql");
            }
        }

        return Tuple.of(true, queryResult, sql);
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
}
