package cn.opensrcdevelop.ai.chat.advisor;

import cn.opensrcdevelop.ai.chat.memory.ChatMemoryContext;
import cn.opensrcdevelop.ai.chat.memory.ChatMemoryContextHolder;
import cn.opensrcdevelop.ai.chat.memory.MultiChatMemoryRepository;
import cn.opensrcdevelop.ai.chat.memory.MultiMessageWindowChatMemory;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.client.ChatClientMessageAggregator;
import org.springframework.ai.chat.client.ChatClientRequest;
import org.springframework.ai.chat.client.ChatClientResponse;
import org.springframework.ai.chat.client.advisor.api.AdvisorChain;
import org.springframework.ai.chat.client.advisor.api.BaseAdvisor;
import org.springframework.ai.chat.client.advisor.api.BaseChatMemoryAdvisor;
import org.springframework.ai.chat.client.advisor.api.StreamAdvisorChain;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Scheduler;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Component
@RequiredArgsConstructor
public class MultiMessageChatMemoryAdvisor implements BaseChatMemoryAdvisor {

    public static final String CHAT_MESSAGE_WINDOW_SIZE = "chat_message_window_size";

    @Value("${ai.chat.memory.message-window-size:20}")
    private Integer defaultMessageWindowSize;

    private final MultiChatMemoryRepository multiChatMemoryRepository;

    @SuppressWarnings("all")
    @Override
    public ChatClientRequest before(ChatClientRequest chatClientRequest, AdvisorChain advisorChain) {
        try {
            ChatMemoryContext chatMemoryContext = new ChatMemoryContext();
            Map<String, Object> requestContext = chatClientRequest.context();
            chatMemoryContext.setPromptTemplate((String) requestContext.get(PromptTemplate.PROMPT_TEMPLATE));
            ChatMemoryContextHolder.setChatMemoryContext(chatMemoryContext);
            ChatMemory chatMemory = getChatMemory((Integer) requestContext.get(CHAT_MESSAGE_WINDOW_SIZE));


            String conversationId = requestContext.get(ChatMemory.CONVERSATION_ID).toString();
            // 1. Retrieve the chat memory for the current conversation.
            List<Message> memoryMessages = chatMemory.get(conversationId);

            // 2. Advise the request messages list.
            List<Message> processedMessages = new ArrayList<>(memoryMessages);
            processedMessages.addAll(chatClientRequest.prompt().getInstructions());

            // 3. Create a new request with the advised messages.
            ChatClientRequest processedChatClientRequest = chatClientRequest.mutate()
                    .prompt(chatClientRequest.prompt().mutate().messages(processedMessages).build())
                    .build();

            // 4. Add the new user message to the conversation memory.
            UserMessage userMessage = processedChatClientRequest.prompt().getUserMessage();
            chatMemory.add(conversationId, userMessage);

            return processedChatClientRequest;
        } finally {
            ChatMemoryContextHolder.removeChatMemoryContext();
        }
    }

    @SuppressWarnings("all")
    @Override
    public ChatClientResponse after(ChatClientResponse chatClientResponse, AdvisorChain advisorChain) {
        try {
            ChatMemoryContext chatMemoryContext = new ChatMemoryContext();
            Map<String, Object> responseContext = chatClientResponse.context();
            chatMemoryContext.setPromptTemplate((String) responseContext.get(PromptTemplate.PROMPT_TEMPLATE));
            ChatMemoryContextHolder.setChatMemoryContext(chatMemoryContext);
            ChatMemory chatMemory = getChatMemory((Integer) responseContext.get(CHAT_MESSAGE_WINDOW_SIZE));

            String conversationId = responseContext.get(ChatMemory.CONVERSATION_ID).toString();
            List<Message> assistantMessages = new ArrayList<>();
            if (chatClientResponse.chatResponse() != null) {
                assistantMessages = chatClientResponse
                        .chatResponse()
                        .getResults()
                        .stream()
                        .map(g -> (Message) g.getOutput())
                        .toList();
            }
            chatMemory.add(conversationId, assistantMessages);
            return chatClientResponse;
        } finally {
            ChatMemoryContextHolder.removeChatMemoryContext();
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
        return -2147482648;
    }

    @NonNull
    @Override
    public Scheduler getScheduler() {
        return BaseAdvisor.DEFAULT_SCHEDULER;
    }

    private ChatMemory getChatMemory(Integer messageWindowSize) {
        if (Objects.isNull(messageWindowSize)) {
            messageWindowSize = defaultMessageWindowSize;
        }
       return new MultiMessageWindowChatMemory(messageWindowSize, multiChatMemoryRepository);
    }
}
