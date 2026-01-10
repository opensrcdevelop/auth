package cn.opensrcdevelop.ai.chat;

import com.alibaba.ttl.TransmittableThreadLocal;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import org.springframework.util.Assert;

public class ChatContextHolder {

    private ChatContextHolder() {
    }

    private static final Map<String, ChatContext> CHAT_CONTEXT_MAP = new ConcurrentHashMap<>();
    private static final TransmittableThreadLocal<ChatContext> CHAT_CONTEXT = new TransmittableThreadLocal<>();

    public static void setChatContext(ChatContext chatContext) {
        Objects.requireNonNull(chatContext);
        Assert.notNull(chatContext.getChatId(), "chatId must not be null");
        CHAT_CONTEXT.set(chatContext);
        CHAT_CONTEXT_MAP.put(chatContext.getChatId(), chatContext);
    }

    public static ChatContext getChatContext() {
        return CHAT_CONTEXT.get();
    }

    public static ChatContext getChatContext(String chatId) {
        return CHAT_CONTEXT_MAP.get(chatId);
    }

    public static void removeChatContext() {
        CHAT_CONTEXT.remove();
    }

    public static void removeChatContext(String chatId) {
        CHAT_CONTEXT_MAP.remove(chatId);
        CHAT_CONTEXT.remove();
    }
}
