package cn.opensrcdevelop.ai.chat;

import com.alibaba.ttl.TransmittableThreadLocal;

public class ChatContext {

    private ChatContext() {}

    private static final TransmittableThreadLocal<String> CHAT_ID = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> QUESTION_ID = new TransmittableThreadLocal<>();

    public static void setChatId(String chatId) {
        CHAT_ID.set(chatId);
    }

    public static String getChatId() {
        return CHAT_ID.get();
    }

    public static void setQuestionId(String questionId) {
        QUESTION_ID.set(questionId);
    }

    public static String getQuestionId() {
        return QUESTION_ID.get();
    }

    public static void clearChatContext() {
        CHAT_ID.remove();
        QUESTION_ID.remove();
    }
}
