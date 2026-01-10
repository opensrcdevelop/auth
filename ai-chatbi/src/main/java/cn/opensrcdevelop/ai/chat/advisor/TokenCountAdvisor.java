package cn.opensrcdevelop.ai.chat.advisor;

import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import java.util.Objects;
import org.springframework.ai.chat.client.ChatClientRequest;
import org.springframework.ai.chat.client.ChatClientResponse;
import org.springframework.ai.chat.client.advisor.api.AdvisorChain;
import org.springframework.ai.chat.client.advisor.api.BaseAdvisor;
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
            if (Objects.nonNull(ChatContextHolder.getChatContext())) {
                ChatContextHolder.getChatContext().getReqTokens()
                        .getAndAdd(chatResponse.getMetadata().getUsage().getPromptTokens());
            }

            if (Objects.nonNull(ChatContextHolder.getChatContext())) {
                ChatContextHolder.getChatContext().getRepTokens()
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
