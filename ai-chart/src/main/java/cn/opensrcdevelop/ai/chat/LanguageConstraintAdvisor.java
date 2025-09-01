package cn.opensrcdevelop.ai.chat;

import org.springframework.ai.chat.client.ChatClientRequest;
import org.springframework.ai.chat.client.ChatClientResponse;
import org.springframework.ai.chat.client.advisor.api.AdvisorChain;
import org.springframework.ai.chat.client.advisor.api.BaseAdvisor;
import org.springframework.ai.chat.messages.SystemMessage;
import org.springframework.ai.chat.prompt.PromptTemplate;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.Ordered;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
public class LanguageConstraintAdvisor implements BaseAdvisor {

    private static final PromptTemplate SYSTEM_PROMPT_TEMPLATE = new PromptTemplate("""
            {instructions}
            Please answer in language: {lang}
            """
    );

    @Value("${ai.chat.language:简体中文}")
    private String language;

    @Override
    @NonNull
    public ChatClientRequest before(@NonNull ChatClientRequest chatClientRequest, @NonNull AdvisorChain advisorChain) {
        SystemMessage systemMessage = chatClientRequest.prompt().getSystemMessage();
        String augmentedSystemText = SYSTEM_PROMPT_TEMPLATE
                .render(Map.of("instructions", systemMessage.getText(), "lang", language));

        return chatClientRequest.mutate()
                .prompt(chatClientRequest.prompt().augmentSystemMessage(augmentedSystemText))
                .build();
    }

    @Override
    @NonNull
    public ChatClientResponse after(@NonNull ChatClientResponse chatClientResponse, @NonNull AdvisorChain advisorChain) {
        return chatClientResponse;
    }

    @Override
    public int getOrder() {
        return Ordered.HIGHEST_PRECEDENCE;
    }
}
