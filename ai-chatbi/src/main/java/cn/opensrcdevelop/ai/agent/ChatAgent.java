package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class ChatAgent {

    private final PromptTemplate promptTemplate;
    private final ChatMessageHistoryService chatMessageHistoryService;

    /**
     * 重写用户提问
     *
     * @param chatClient
     *            ChatClient
     * @param userQuestion
     *            用户提问
     * @param instruction
     *            指令
     * @return 重写后的用户提问
     */
    public Map<String, Object> rewriteUserQuestion(ChatClient chatClient, String userQuestion, String instruction) {
        // 1. 获取用户历史提问
        List<String> userQuestions = chatMessageHistoryService
                .getUserHistoryQuestions(ChatContextHolder.getChatContext().getChatId());
        if (CollectionUtils.isEmpty(userQuestions) || userQuestions.size() < 2) {
            return Map.of(
                    "success", true,
                    "rewritten_question", userQuestion);
        }

        // 2. 重写用户提问
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.REWRITE_QUESTION)
                .param("historical_questions", new ArrayList<>(userQuestions))
                .param("original_question", userQuestion)
                .param("instruction", instruction);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.REWRITE_QUESTION))
                .user(prompt.buildUserPrompt(PromptTemplate.REWRITE_QUESTION))
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.REWRITE_QUESTION))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 提取用户提问中的查询信息
     *
     * @param chatClient
     *            ChatClient
     * @param userQuestion
     *            用户提问
     * @param instruction
     *            指令
     * @return 查询信息
     */
    public Map<String, Object> extractQuery(ChatClient chatClient, String userQuestion, String instruction) {
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.EXTRACT_QUERY)
                .param("question", userQuestion)
                .param("instruction", instruction);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.EXTRACT_QUERY))
                .user(prompt.buildUserPrompt(PromptTemplate.EXTRACT_QUERY))
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.EXTRACT_QUERY))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 批量判断历史问题与当前问题的关联性，返回相关的问题-SQL 对
     *
     * @param chatClient
     *            ChatClient
     * @param currentQuestion
     *            当前问题
     * @param historicalPairs
     *            历史问题-SQL 对列表
     * @param maxSamples
     *            返回的最大数量
     * @return 相关的问题-SQL 对列表
     */
    public List<Map<String, String>> filterRelatedHistoricalAnswers(ChatClient chatClient,
            String currentQuestion,
            List<Map<String, String>> historicalPairs,
            int maxSamples) {
        if (historicalPairs == null || historicalPairs.isEmpty()) {
            return new ArrayList<>();
        }

        // 构建批量判断的提示词
        StringBuilder sb = new StringBuilder();
        sb.append("当前问题: ").append(currentQuestion).append("\n\n");
        sb.append("历史问题-SQL 对:\n");
        for (int i = 0; i < historicalPairs.size(); i++) {
            Map<String, String> pair = historicalPairs.get(i);
            sb.append(i + 1).append(". 问题: ").append(pair.get("question")).append("\n");
            sb.append("   SQL: ").append(pair.get("sql")).append("\n\n");
        }
        sb.append("请判断上述哪些历史问题与当前问题语义相关，返回相关的问题-SQL 对（最多").append(maxSamples).append("个）。\n");
        sb.append("只返回 JSON 数组格式，例如: [{\"question\": \"问题1\", \"sql\": \"SQL1\"}, ...]\n");
        sb.append("如果不相关，返回空数组: []");

        try {
            String result = chatClient.prompt()
                    .system("你是一个问题关联性判断助手。需要判断历史问题与当前问题是否语义相关。")
                    .user(sb.toString())
                    .call()
                    .content();

            // 解析返回的 JSON 数组
            return parseRelatedHistoricalAnswers(result, maxSamples);
        } catch (Exception e) {
            return new ArrayList<>();
        }
    }

    private List<Map<String, String>> parseRelatedHistoricalAnswers(String result, int maxSamples) {
        List<Map<String, String>> related = new ArrayList<>();
        try {
            // 简单解析 JSON 数组
            result = result.trim();
            if (result.startsWith("[")) {
                com.fasterxml.jackson.databind.ObjectMapper mapper = new com.fasterxml.jackson.databind.ObjectMapper();
                List<Map<String, String>> parsed = mapper.readValue(result,
                        new com.fasterxml.jackson.core.type.TypeReference<List<Map<String, String>>>() {
                        });
                for (Map<String, String> item : parsed) {
                    if (related.size() >= maxSamples) {
                        break;
                    }
                    if (item.containsKey("question") && item.containsKey("sql")) {
                        related.add(item);
                    }
                }
            }
        } catch (Exception e) {
            // 解析失败返回空列表
        }
        return related;
    }
}
