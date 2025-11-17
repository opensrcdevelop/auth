package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.ChatMessageHistoryService;
import lombok.RequiredArgsConstructor;
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


    /**
     * 重写用户提问
     *
     * @param chatClient   ChatClient
     * @param userQuestion 用户提问
     * @return 重写后的用户提问
     */
    public Map<String, Object> rewriteUserQuestion(ChatClient chatClient, String userQuestion) {
        // 1. 获取用户历史提问
        List<String> userQuestions = chatMessageHistoryService.getUserHistoryQuestions(ChatContextHolder.getChatContext().getChatId());

        // 2. 重写用户提问
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.REWRITE_QUESTION)
                .param("historical_questions", new ArrayList<>(userQuestions))
                .param("original_question", userQuestion);

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
     * @param chatClient   ChatClient
     * @param userQuestion 用户提问
     * @return 查询信息
     */
    public Map<String, Object> extractQuery(ChatClient chatClient, String userQuestion) {
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.EXTRACT_QUERY)
                .param("question", userQuestion);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt(PromptTemplate.EXTRACT_QUERY))
                .user(prompt.buildUserPrompt(PromptTemplate.EXTRACT_QUERY))
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.EXTRACT_QUERY))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {});
    }
}