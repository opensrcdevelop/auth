package cn.opensrcdevelop.ai.service.impl;

import cn.opensrcdevelop.ai.chat.ChatClientManager;
import cn.opensrcdevelop.ai.datasource.DataSourceManager;
import cn.opensrcdevelop.ai.dto.ChatBIRequestDto;
import cn.opensrcdevelop.ai.service.ChatBIService;
import cn.opensrcdevelop.ai.text2sql.Text2SQLAgent;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.ai.util.Text2SqlUtil;
import cn.opensrcdevelop.common.constants.ExecutorConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.stereotype.Service;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.IOException;
import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.Executor;

@Slf4j
@Service
@RequiredArgsConstructor
public class ChatBIServiceImpl implements ChatBIService {

    private static final Long CHAT_TIMEOUT = Duration.ofMinutes(60).toMillis();

    private final DataSourceManager dataSourceManager;
    private final ChatClientManager chatClientManager;
    private final Text2SQLAgent text2SQLAgent;

    @Resource(name = ExecutorConstants.EXECUTOR_IO_DENSE)
    private Executor executor;

    @Override
    public SseEmitter streamChatBI(ChatBIRequestDto requestDto) {
        SseEmitter emitter = new SseEmitter(CHAT_TIMEOUT);

        executor.execute(() -> {
            String chatId = Objects.isNull(requestDto.getChatId()) ? CommonUtil.getUUIDV7String() : requestDto.getChatId();
            try {
                processStreamRequest(requestDto, emitter, chatId);
                SseUtil.sendChatBIDone(emitter);
            } catch (Exception ex) {
                emitter.completeWithError(ex);
            } finally {
                emitter.complete();
            }
        });

        return emitter;
    }

    private void  processStreamRequest(ChatBIRequestDto requestDto, SseEmitter emitter, String chatId) throws IOException {
        String dataSourceId = requestDto.getDataSourceId();

        // 1. 获取 ChatClient
        ChatClient chatClient = chatClientManager.getChatClient(requestDto.getModelProviderId(), requestDto.getModel(), chatId);

        // 2. 根据用户问题获取关联的表信息
        List<Map<String, Object>> relevantTables = text2SQLAgent.getRelevantTables(chatClient, requestDto.getQuestion(), dataSourceId);
        SseUtil.sendChatBIText(emitter, "基于用户输入，");
        SseUtil.sendChatBIText(emitter, "匹配到以下表：\n");
        for (Map<String, Object> table : relevantTables) {
            SseUtil.sendChatBIMd(emitter, "`%s` ".formatted(table.get("table_name")));
        }

        // 3. 生成 SQL
        String sql = text2SQLAgent.generateSql(chatClient, requestDto.getQuestion(), relevantTables, dataSourceId);
        if (StringUtils.isEmpty(sql)) {
            SseUtil.sendChatBIText(emitter, "无法生成有效的 SQL 语句");
            return;
        }
        SseUtil.sendChatBIText(emitter, "生成的 SQL 语句：\n");
        SseUtil.sendChatBIMd(emitter, sql);
        SseUtil.sendChatBIText(emitter, "\n");

        // 4. 执行 SQL
        String execSql = Text2SqlUtil.extractSql(sql);
        List<Map<String, Object>> queryResult = Collections.emptyList();
        try {
            queryResult = dataSourceManager.getJdbcTemplate(dataSourceId).queryForList(execSql);
            if (CollectionUtils.isEmpty(queryResult)) {
                SseUtil.sendChatBIText(emitter, "查询未返回数据");
                return;
            }
        } catch (Exception ex) {
            log.error(ex.getMessage(), ex);
            SseUtil.sendChatBIText(emitter, "执行 SQL 失败");
            return;
        }

        // 5. 生成 Echarts 图表配置
        Map<String, Object> echartsConfig = text2SQLAgent.generateEchartsConfig(
                chatClient,
                requestDto.getQuestion(),
                execSql,
                CommonUtil.serializeObject(queryResult));
        if (Objects.isNull(echartsConfig)) {
            SseUtil.sendChatBIText(emitter, "无法生成有效的 Echarts 配置");
            return;
        }
        SseUtil.sendChatBIEcharts(emitter, echartsConfig);
    }
}
