package cn.opensrcdevelop.ai.chat.memory;

import com.alibaba.ttl.TransmittableThreadLocal;

public class ChatMemoryContext {

    private ChatMemoryContext () {}

    private static final TransmittableThreadLocal<String> PROMPT_TEMPLATE = new TransmittableThreadLocal<>();


    public static void setPromptTemplate(String promptTemplate) {
        PROMPT_TEMPLATE.set(promptTemplate);
    }

    public static String getPromptTemplate() {
        return PROMPT_TEMPLATE.get();
    }

    public static void clearChatMemoryContext() {
        PROMPT_TEMPLATE.remove();
    }
}
