package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.ai.service.MultiChatMemoryService;
import cn.opensrcdevelop.ai.service.TableService;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class ChatAgent {

    private final PromptTemplate promptTemplate;
    private final TableService tableService;
    private final MultiChatMemoryService multiChatMemoryService;


    /**
     * 重写用户提问
     *
     * @param chatClient     ChatClient
     * @param userQuestion   用户提问
     * @param relevantTables 关联表
     * @return 重写后的用户提问
     */
    public Map<String, Object> rewriteUserQuestion(ChatClient chatClient, String userQuestion) {
        // 1. 获取用户历史提问
        List<String> userQuestions = multiChatMemoryService.getUserHistoryQuestions(ChatContext.getChatId());
        if (CollectionUtils.isEmpty(userQuestions)) {
            return Map.of(
                    "success", false
            );
        }

        // 3. 重写用户提问
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.REWRITE_QUESTION)
                .param("historical_questions", userQuestions)
                .param("original_question", userQuestion);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.REWRITE_QUESTION))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }
}
