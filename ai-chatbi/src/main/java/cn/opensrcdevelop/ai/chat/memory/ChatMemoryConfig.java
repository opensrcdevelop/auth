package cn.opensrcdevelop.ai.chat.memory;

import org.springframework.ai.chat.memory.ChatMemory;
import org.springframework.ai.chat.memory.MessageWindowChatMemory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ChatMemoryConfig {

    @Value("${ai.chat.memory.message-window-size:20}")
    private int messageWindowSize;

    @Bean
    public ChatMemory chatMemory(MultiChatMemoryRepository multiChatMemoryRepository) {
        return MessageWindowChatMemory.builder()
                .chatMemoryRepository(multiChatMemoryRepository)
                .maxMessages(messageWindowSize)
                .build();
    }
}
