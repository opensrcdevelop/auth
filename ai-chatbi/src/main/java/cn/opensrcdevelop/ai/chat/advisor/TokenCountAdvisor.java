package cn.opensrcdevelop.ai.chat.advisor;

import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import java.util.Objects;
import org.springframework.ai.chat.client.ChatClientRequest;
import org.springframework.ai.chat.client.ChatClientResponse;
import org.springframework.ai.chat.client.advisor.api.AdvisorChain;
import org.springframework.ai.chat.client.advisor.api.BaseAdvisor;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.core.Ordered;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

@Component
public class TokenCountAdvisor implements BaseAdvisor {

    @Override
    @NonNull
    public ChatClientRequest before(@NonNull ChatClientRequest chatClientRequest, @NonNull AdvisorChain advisorChain) {
        return chatClientRequest;
    }

    @Override
    @NonNull
    public ChatClientResponse after(@NonNull ChatClientResponse chatClientResponse,
            @NonNull AdvisorChain advisorChain) {
        ChatResponse chatResponse = chatClientResponse.chatResponse();
        if (Objects.nonNull(chatResponse)) {
            ChatContext chatContext = ChatContextHolder.getChatContext();
            if (Objects.isNull(chatContext)) {
                chatContext = ChatContextHolder
                        .getChatContext((String) chatClientResponse.context().get(ChatMemory.CONVERSATION_ID));
            }

            if (Objects.nonNull(chatContext)) {
                chatContext.getInputTokens()
                        .getAndAdd(chatResponse.getMetadata().getUsage().getPromptTokens());
            }

            if (Objects.nonNull(chatContext)) {
                chatContext.getOutputTokens()
                        .getAndAdd(chatResponse.getMetadata().getUsage().getCompletionTokens());
            }
        }
        return chatClientResponse;
    }

    @Override
    public int getOrder() {
        return Ordered.HIGHEST_PRECEDENCE + 10;
    }
}
