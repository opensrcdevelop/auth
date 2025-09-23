package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.model.ChartRecord;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class AnalyzeAgent {

    private final PromptTemplate promptTemplate;

    /**
     * 分析图表数据
     *
     * @param chatClient  ChatClient
     * @param chartRecord 临时图表记录
     * @return 分析结果
     */
    public Map<String, Object> analyzeData(ChatClient chatClient, ChartRecord chartRecord) {
        // 1. 分析图表数据
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.ANALYZE_DATA)
                .param("user_query", chartRecord.getQuestion())
                .param("query_result", CommonUtil.serializeObject(chartRecord.getData()))
                .param("column_aliases", CommonUtil.serializeObject(chartRecord.getColumns()));

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 生成分析报告
     *
     * @param chatClient      ChatClient
     * @param chartRecord     临时图表记录
     * @param analysisResults 分析结果
     * @return 分析报告
     */
    public Map<String, Object> generateAnalysisReport(ChatClient chatClient, ChartRecord chartRecord, List<String> analysisResults) {
        // 1. 生成分析报告
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.GENERATE_REPORT)
                .param("user_query", chartRecord.getQuestion())
                .param("query_result", CommonUtil.serializeObject(chartRecord.getData()))
                .param("column_aliases", CommonUtil.serializeObject(chartRecord.getColumns()))
                .param("analysis_results", CommonUtil.serializeObject(analysisResults))
                .param("current_time", LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)));

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }
}
