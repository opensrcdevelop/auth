package cn.opensrcdevelop.ai.chat.memory;

import lombok.RequiredArgsConstructor;
import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.messages.Message;

import java.util.List;

@SuppressWarnings("all")
@RequiredArgsConstructor
public class MultiMessageWindowChatMemory implements ChatMemory {

    private final Integer windowSize;
    private final MultiChatMemoryRepository chatMemoryRepository;

    @Override
    public void add(String conversationId, Message message) {
        chatMemoryRepository.saveAll(conversationId, List.of(message));
    }

    @Override
    public void add(String conversationId, List<Message> messages) {
        chatMemoryRepository.saveAll(conversationId, messages);
    }

    @Override
    public List<Message> get(String conversationId) {
        return chatMemoryRepository.findByConversationId(conversationId, windowSize);
    }

    @Override
    public void clear(String conversationId) {
        chatMemoryRepository.deleteByConversationId(conversationId);
    }
}
