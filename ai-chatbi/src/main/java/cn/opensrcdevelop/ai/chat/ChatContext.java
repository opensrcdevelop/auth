package cn.opensrcdevelop.ai.chat;

import com.alibaba.ttl.TransmittableThreadLocal;
import org.springframework.ai.chat.client.ChatClient;

import java.util.List;
import java.util.Map;

public class ChatContext {

    private ChatContext() {
    }

    private static final TransmittableThreadLocal<String> CHAT_ID = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> QUESTION_ID = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> QUESTION = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> SQL = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<List<Map<String, Object>>> QUERY_DATA = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<ChatClient> CHAT_CLIENT = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<List<Map<String, Object>>> QUERY_COLUMNS = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> ANALYZE_DATA_SUMMARY = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> ANALYZE_DATA_RESULT = new TransmittableThreadLocal<>();


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

    public static void setQuestion(String question) {
        QUESTION.set(question);
    }

    public static String getQuestion() {
        return QUESTION.get();
    }

    public static void setSql(String sql) {
        SQL.set(sql);
    }

    public static String getSql() {
        return SQL.get();
    }

    public static void setQueryData(List<Map<String, Object>> queryData) {
        QUERY_DATA.set(queryData);
    }

    public static List<Map<String, Object>> getQueryData() {
        return QUERY_DATA.get();
    }

    public static void setChatClient(ChatClient chatClient) {
        CHAT_CLIENT.set(chatClient);
    }

    public static ChatClient getChatClient() {
        return CHAT_CLIENT.get();
    }

    public static void setQueryColumns(List<Map<String, Object>> columns) {
        QUERY_COLUMNS.set(columns);
    }

    public static List<Map<String, Object>> getQueryColumns() {
        return QUERY_COLUMNS.get();
    }

    public static void setAnalyzeDataSummary(String analyzeDataSummary) {
        ANALYZE_DATA_SUMMARY.set(analyzeDataSummary);
    }

    public static String getAnalyzeDataSummary() {
        return ANALYZE_DATA_SUMMARY.get();
    }

    public static void setAnalyzeDataResult(String analyzeDataResult) {
        ANALYZE_DATA_RESULT.set(analyzeDataResult);
    }

    public static String getAnalyzeDataResult() {
        return ANALYZE_DATA_RESULT.get();
    }

    public static void clearChatContext() {
        CHAT_ID.remove();
        QUESTION_ID.remove();
        SQL.remove();
        QUERY_DATA.remove();
        QUESTION.remove();
        CHAT_CLIENT.remove();
        QUERY_COLUMNS.remove();
        ANALYZE_DATA_SUMMARY.remove();
        ANALYZE_DATA_RESULT.remove();
    }
}
