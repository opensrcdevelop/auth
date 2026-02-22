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
     * 批量判断历史问题与当前问题的关联性，返回相关的 answerId 列表
     *
     * @param chatClient
     *            ChatClient
     * @param currentQuestion
     *            当前问题
     * @param historicalAnswers
     *            历史回答列表（包含 answerId 和 question）
     * @param maxSamples
     *            返回的最大数量
     * @return 相关的 answerId 列表
     */
    public Map<String, Object> filterRelatedHistoricalAnswers(ChatClient chatClient,
            String currentQuestion,
            List<Map<String, String>> historicalAnswers,
            int maxSamples) {
        if (historicalAnswers == null || historicalAnswers.isEmpty()) {
            return Map.of(
                    "success", true,
                    "related_answer_ids", new ArrayList<>());
        }

        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.FILTER_RELATED_HISTORICAL_ANSWERS)
                .param("question", currentQuestion)
                .param("historical_answers", historicalAnswers)
                .param("max_samples", maxSamples);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.FILTER_RELATED_HISTORICAL_ANSWERS))
                .user(prompt.buildUserPrompt(PromptTemplate.FILTER_RELATED_HISTORICAL_ANSWERS))
                .advisors(
                        a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.FILTER_RELATED_HISTORICAL_ANSWERS))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }
}
