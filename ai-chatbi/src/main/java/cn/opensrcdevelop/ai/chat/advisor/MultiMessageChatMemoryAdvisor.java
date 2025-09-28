package cn.opensrcdevelop.ai.chat.advisor;

import cn.opensrcdevelop.ai.chat.memory.ChatMemoryContext;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import org.springframework.ai.chat.client.ChatClientMessageAggregator;
import org.springframework.ai.chat.client.ChatClientRequest;
import org.springframework.ai.chat.client.ChatClientResponse;
import org.springframework.ai.chat.client.advisor.MessageChatMemoryAdvisor;
import org.springframework.ai.chat.client.advisor.api.AdvisorChain;
import org.springframework.ai.chat.client.advisor.api.BaseChatMemoryAdvisor;
import org.springframework.ai.chat.client.advisor.api.StreamAdvisorChain;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

@Component
public class MultiMessageChatMemoryAdvisor implements BaseChatMemoryAdvisor {

    private final MessageChatMemoryAdvisor delegate;

    public MultiMessageChatMemoryAdvisor(ChatMemory chatMemory) {
        this.delegate = MessageChatMemoryAdvisor.builder(chatMemory).build();
    }

    @NonNull
    @Override
    public ChatClientRequest before(ChatClientRequest chatClientRequest, @NonNull AdvisorChain advisorChain) {
        try {
            ChatMemoryContext.setPromptTemplate((String) chatClientRequest.context().get(PromptTemplate.PROMPT_TEMPLATE));
            return delegate.before(chatClientRequest, advisorChain);
        } finally {
            ChatMemoryContext.clearChatMemoryContext();
        }
    }

    @NonNull
    @Override
    public ChatClientResponse after(ChatClientResponse chatClientResponse, @NonNull AdvisorChain advisorChain) {
        try {
            ChatMemoryContext.setPromptTemplate((String) chatClientResponse.context().get(PromptTemplate.PROMPT_TEMPLATE));
            return delegate.after(chatClientResponse, advisorChain);
        } finally {
            ChatMemoryContext.clearChatMemoryContext();
        }
    }

    @NonNull
    @Override
    public Flux<ChatClientResponse> adviseStream(@NonNull ChatClientRequest chatClientRequest, @NonNull StreamAdvisorChain streamAdvisorChain) {
        // Get the scheduler from BaseAdvisor
        Scheduler scheduler = this.getScheduler();

        // Process the request with the before method
        return Mono.just(chatClientRequest)
                .publishOn(scheduler)
                .map(request -> this.before(request, streamAdvisorChain))
                .flatMapMany(streamAdvisorChain::nextStream)
                .transform(flux -> new ChatClientMessageAggregator().aggregateChatClientResponse(flux,
                        response -> this.after(response, streamAdvisorChain)));
    }

    @Override
    public int getOrder() {
        return delegate.getOrder();
    }

    @NonNull
    @Override
    public Scheduler getScheduler() {
        return delegate.getScheduler();
    }
}
