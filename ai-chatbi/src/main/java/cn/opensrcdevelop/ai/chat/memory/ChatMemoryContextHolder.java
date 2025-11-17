package cn.opensrcdevelop.ai.chat.memory;

import com.alibaba.ttl.TransmittableThreadLocal;

public class ChatMemoryContextHolder {

    private ChatMemoryContextHolder() {
    }

    private static final TransmittableThreadLocal<ChatMemoryContext> CHAT_MEMORY_CONTEXT = new TransmittableThreadLocal<>();

    public static void setChatMemoryContext(ChatMemoryContext chatMemoryContext) {
        CHAT_MEMORY_CONTEXT.set(chatMemoryContext);
    }

    public static ChatMemoryContext getChatMemoryContext() {
        return CHAT_MEMORY_CONTEXT.get();
    }

     public static void removeChatMemoryContext() {
        CHAT_MEMORY_CONTEXT.remove();
    }
}
