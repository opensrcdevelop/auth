package cn.opensrcdevelop.ai.constants;

public class RedisTopicConstants {
    private RedisTopicConstants() {
    }

    public static final String USER_ANSWER_TOPIC = "chat:user-answer:";

    public static String getTopic(String chatId) {
        return USER_ANSWER_TOPIC + chatId;
    }
}
