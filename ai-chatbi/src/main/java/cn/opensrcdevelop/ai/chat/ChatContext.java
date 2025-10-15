package cn.opensrcdevelop.ai.chat;

import com.alibaba.ttl.TransmittableThreadLocal;
import org.springframework.ai.chat.client.ChatClient;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

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

    private static final TransmittableThreadLocal<AtomicInteger> REQ_TOKENS = TransmittableThreadLocal.withInitial(AtomicInteger::new);

    private static final TransmittableThreadLocal<AtomicInteger> REP_TOKENS = TransmittableThreadLocal.withInitial(AtomicInteger::new);

    private static final TransmittableThreadLocal<Map<String, Object>> CHART_CONFIG = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> REPORT_TYPE = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> REPORT = new TransmittableThreadLocal<>();

    private static final TransmittableThreadLocal<String> RAW_QUESTION = new TransmittableThreadLocal<>();


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

    public static void setReqTokens(int reqTokens) {
        REQ_TOKENS.get().addAndGet(reqTokens);
    }

    public static int getReqTokens() {
        return REQ_TOKENS.get().get();
    }

    public static void setRepTokens(int repTokens) {
        REP_TOKENS.get().addAndGet(repTokens);
    }

    public static int getRepTokens() {
        return REP_TOKENS.get().get();
    }

    public static void setChartConfig(Map<String, Object> chartConfig) {
        CHART_CONFIG.set(chartConfig);
    }

    public static Map<String, Object> getChartConfig() {
        return CHART_CONFIG.get();
    }

    public static void setReportType(String reportType) {
        REPORT_TYPE.set(reportType);
    }

    public static String getReportType() {
        return REPORT_TYPE.get();
    }

    public static void setReport(String report) {
        REPORT.set(report);
    }

    public static String getReport() {
        return REPORT.get();
    }

     public static void setRawQuestion(String rawQuestion) {
         RAW_QUESTION.set(rawQuestion);
    }

    public static String getRawQuestion() {
        return RAW_QUESTION.get();
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
        REQ_TOKENS.remove();
        REP_TOKENS.remove();
        CHART_CONFIG.remove();
        REPORT_TYPE.remove();
        REPORT.remove();
        RAW_QUESTION.remove();
    }
}
