package cn.opensrcdevelop.ai.chat;

import lombok.Data;
import org.springframework.ai.chat.client.ChatClient;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

@Data
public class ChatContext {

    private ChatClient chatClient;

    private String dataSourceId;

    private String chatId;

    private String questionId;

    private String question;

    private String rawQuestion;

    private String userQuery;

    private List<Map<String, Object>> relevantTables;

    private String sql;

    private List<Map<String, Object>> queryData;

    private List<Map<String, Object>> queryColumns;

    private String analyzeDataSummary;

    private String analyzeDataResult;

    private Map<String, Object> chartConfig;

    private String reportType;

    private String report;

    private AtomicInteger reqTokens = new AtomicInteger(0);

    private AtomicInteger repTokens = new AtomicInteger(0);

    private List<Map<String, Object>> toolCallResults;
}
