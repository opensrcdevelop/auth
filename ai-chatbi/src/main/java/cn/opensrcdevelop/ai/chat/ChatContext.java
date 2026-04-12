package cn.opensrcdevelop.ai.chat;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import lombok.Data;
import org.springframework.ai.chat.client.ChatClient;

@Data
public class ChatContext {

    private String id;

    private ChatClient chatClient;

    private String dataSourceId;

    private String chatId;

    private String questionId;

    private String question;

    private String rawQuestion;

    private String userQuery;

    private List<String> relevantTableIds;

    private String sql;

    private List<Map<String, Object>> queryData;

    private List<Map<String, Object>> queryColumns;

    private String analyzeDataSummary;

    private String analyzeDataResult;

    private Map<String, Object> chartConfig;

    private String reportType;

    private String report;

    private AtomicInteger inputTokens = new AtomicInteger(0);

    private AtomicInteger outputTokens = new AtomicInteger(0);

    private List<Map<String, Object>> toolCallResults;

    /** 上一轮的思考内容，用于连贯推理 */
    private String previousThinking;

    /** 示例 SQL（问题-SQL 对列表） */
    private List<Map<String, String>> sampleSqls;

    private Boolean terminated = false;

    /** 临时文件路径（存储超阈值查询结果） */
    private List<String> queryResultFilePaths = new ArrayList<>();

    public void addQueryResultFilePath(String path) {
        if (queryResultFilePaths == null) {
            queryResultFilePaths = new ArrayList<>();
        }
        queryResultFilePaths.add(path);
    }

    public void clearQueryResultFilePaths() {
        if (queryResultFilePaths != null) {
            queryResultFilePaths.clear();
        }
    }
}
