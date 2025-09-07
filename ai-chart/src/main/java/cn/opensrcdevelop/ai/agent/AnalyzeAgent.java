package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.model.ChartRecord;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.util.PromptTemplateUtil;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@RequiredArgsConstructor
public class AnalyzeAgent {

    private final PromptTemplate promptTemplate;


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
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }
}
