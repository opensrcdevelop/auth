package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.agent.ChartAgent;
import cn.opensrcdevelop.ai.agent.SqlAgent;
import cn.opensrcdevelop.ai.chat.ChatClientManager;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.entity.ChartConf;
import cn.opensrcdevelop.ai.service.ChartConfService;
import cn.opensrcdevelop.ai.service.ChatBIService;
import cn.opensrcdevelop.ai.util.ChartRenderer;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
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

@Slf4j
@Service
@RequiredArgsConstructor
public class ChatBIServiceImpl implements ChatBIService {

    private static final Long CHAT_TIMEOUT = Duration.ofMinutes(60).toMillis();

    private final ChartConfService chartConfService;
    private final DataSourceManager dataSourceManager;
    private final ChatClientManager chatClientManager;
    private final SqlAgent sqlAgent;
    private final ChartAgent chartAgent;

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
                processStreamRequest(requestDto, emitter, chatId);
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
    private void  processStreamRequest(ChatBIRequestDto requestDto, SseEmitter emitter, String chatId) throws IOException {
        String dataSourceId = requestDto.getDataSourceId();

        // 1. 获取 ChatClient
        ChatClient chatClient = chatClientManager.getChatClient(requestDto.getModelProviderId(), requestDto.getModel(), chatId);

        // 2. 根据用户问题获取关联的表信息
        SseUtil.sendChatBILoading(emitter, "正在匹配相关表信息...");
        Map<String, Object> tableResult = sqlAgent.getRelevantTables(chatClient, requestDto.getQuestion(), dataSourceId);
        if (!Boolean.TRUE.equals(tableResult.get("success"))) {
            SseUtil.sendChatBIText(emitter, "无法获取相关表信息，原因：%s".formatted(tableResult.get("error")));
            return;
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
            SseUtil.sendChatBIText(emitter, "无法生成 SQL，原因：%s".formatted(tableResult.get("error")));
            return;
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
            return;
        }

        List<Map<String, Object>> queryResult = executeResult._2;
        if (CollectionUtils.isEmpty(queryResult)) {
            SseUtil.sendChatBIText(emitter, "查询未返回数据");
            return;
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
            SseUtil.sendChatBIText(emitter, "无法生成图表，原因：%s".formatted(chartConfResult.get("error")));
            return;
        }

        // 6. 保存图表配置
        ChartConf chartConf = new ChartConf();
        chartConf.setChartId(CommonUtil.getUUIDV7String());
        chartConf.setDataSourceId(dataSourceId);
        chartConf.setChatId(chatId);
        chartConf.setSql(sql);
        chartConf.setConfig(CommonUtil.serializeObject(chartConfResult.get("config")));
        chartConfService.save(chartConf);

        // 7. 生成图表
        SseUtil.sendChatBIText(emitter, "已生成的图表：");
        SseUtil.sendChatBIChart(emitter, ChartRenderer.render(chartConf, queryResult));
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
}
