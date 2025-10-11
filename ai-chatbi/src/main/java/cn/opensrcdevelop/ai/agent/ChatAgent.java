package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.tool.AnalyzeDataTool;
import cn.opensrcdevelop.ai.chat.tool.GenerateChartTool;
import cn.opensrcdevelop.ai.chat.tool.GenerateReportTool;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class ChatAgent {

    private final PromptTemplate promptTemplate;
    private final ChatMessageHistoryService chatMessageHistoryService;
    private final AnalyzeDataTool analyzeDataTool;
    private final GenerateChartTool generateChartTool;
    private final GenerateReportTool generateReportTool;


    /**
     * 重写用户提问
     *
     * @param chatClient   ChatClient
     * @param userQuestion 用户提问
     * @return 重写后的用户提问
     */
    public Map<String, Object> rewriteUserQuestion(ChatClient chatClient, String userQuestion) {
        // 1. 获取用户历史提问
        List<String> userQuestions = chatMessageHistoryService.getUserHistoryQuestions(ChatContext.getChatId());
        if (CollectionUtils.isEmpty(userQuestions)) {
            return Map.of(
                    "success", false
            );
        }

        // 2. 重写用户提问
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.REWRITE_QUESTION)
                .param("historical_questions", new ArrayList<>(userQuestions))
                .param("original_question", userQuestion);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.REWRITE_QUESTION))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 回答用户提问
     *
     * @param chatClient   ChatClient
     * @param userQuestion 用户提问
     * @param queryResult  查询结果
     * @param queryColumns 查询列别名
     * @return 回答用户提问的结果
     */
    public Map<String, Object> answerQuestion(ChatClient chatClient,
                                              String userQuestion,
                                              List<Map<String, Object>> queryResult,
                                              List<Map<String, Object>> queryColumns) {

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.ANSWER_QUESTION)
                .param("question", userQuestion)
                .param("query_result", CommonUtil.serializeObject(queryResult))
                .param("column_aliases", CommonUtil.serializeObject(queryColumns));

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.ANSWER_QUESTION))
                .tools(analyzeDataTool, generateChartTool, generateReportTool)
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 提取用户提问中的查询信息
     *
     * @param chatClient   ChatClient
     * @param userQuestion 用户提问
     * @return 查询信息
     */
    public Map<String, Object> extractQuery(ChatClient chatClient, String userQuestion) {
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.EXTRACT_QUERY)
                .param("question", userQuestion);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.EXTRACT_QUERY))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }
}
