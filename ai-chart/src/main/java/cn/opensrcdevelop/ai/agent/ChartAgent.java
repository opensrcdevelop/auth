package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.util.PromptTemplateUtil;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class ChartAgent {

    private final PromptTemplate promptTemplate;


    /**
     * 根据用户提问及数据库查询结果生成图表
     *
     * @param chatClient  ChatClient
     * @param sql 执行的 SQL
     * @param question 用户提问
     * @param queryResult 查询结果
     * @return 图表配置
     */
    public Map<String, Object> generateChart(ChatClient chatClient,
                                String sql,
                                String question,
                                List<Map<String, Object>> queryResult) {
        // 1. 生成图表配置
        PromptTemplate.Prompt prompt = promptTemplate.getTemplates().get("generate_chart");
        return chatClient.prompt()
                .system(prompt.getSystem())
                .user(PromptTemplateUtil.getPrompt(
                        prompt.getUser(),
                        Map.of(
                                "user_query", question,
                                "sql", sql,
                                "query_result", CommonUtil.serializeObject(queryResult)
                        )
                ))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }
}
