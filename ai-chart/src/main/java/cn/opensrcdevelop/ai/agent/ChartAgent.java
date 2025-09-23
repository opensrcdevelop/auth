package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
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
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.GENERATE_CHART)
                .param("user_query", question)
                .param("sql", sql)
                .param("query_result", CommonUtil.serializeObject(queryResult));

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }
}
