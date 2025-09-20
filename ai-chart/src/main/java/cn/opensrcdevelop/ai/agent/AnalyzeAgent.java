package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.model.ChartRecord;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.util.PromptTemplateUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;

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
        PromptTemplate.Prompt prompt = promptTemplate.getTemplates().get("analyze_data");
        return chatClient.prompt()
                .system(prompt.getSystem())
                .user(PromptTemplateUtil.getPrompt(
                        prompt.getUser(),
                        Map.of(
                                "user_query", chartRecord.getQuestion(),
                                "query_result", CommonUtil.serializeObject(chartRecord.getData()),
                                "column_aliases", CommonUtil.serializeObject(chartRecord.getColumns())
                        )
                ))
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
    public Flux<String> generateAnalysisReport(ChatClient chatClient, ChartRecord chartRecord, List<String> analysisResults) {
        // 2. 生成分析报告
        PromptTemplate.Prompt prompt = promptTemplate.getTemplates().get("generate_report");
        return chatClient.prompt()
                .system(prompt.getSystem())
                .user(PromptTemplateUtil.getPrompt(
                        prompt.getUser(),
                        Map.of(
                                "user_query", chartRecord.getQuestion(),
                                "query_result", CommonUtil.serializeObject(chartRecord.getData()),
                                "column_aliases", CommonUtil.serializeObject(chartRecord.getColumns()),
                                "analysis_results", analysisResults,
                                "current_time", LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS))
                        )
                ))
                .stream()
                .content();
    }
}
