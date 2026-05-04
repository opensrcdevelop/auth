package cn.opensrcdevelop.ai.chat;

import cn.opensrcdevelop.ai.dto.ChatConfigDto;
import com.fasterxml.jackson.annotation.JsonIgnore;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import lombok.Data;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

@Data
public class ChatContext {

    /** 对话上下文 ID */
    private String id;

    /** 对话客户端 */
    private ChatClient chatClient;

    @JsonIgnore
    private SseEmitter emitter;

    /** 模型提供商ID */
    private String modelProviderId;

    /** 模型 */
    private String model;

    /** 数据源 ID */
    private String dataSourceId;

    /** 对话 ID */
    private String chatId;

    /** 问题 ID */
    private String questionId;

    /** 问题内容 */
    private String question;

    /** 原始问题内容 */
    private String rawQuestion;

    /** 用户查询内容 */
    private String userQuery;

    /** 相关表 ID 列表 */
    private List<String> relevantTableIds;

    /** 生成的 SQL 语句 */
    private String sql;

    /** 查询结果数据 */
    private List<Map<String, Object>> queryData;

    /** 查询结果列信息 */
    private List<Map<String, Object>> queryColumns;

    /** 数据分析摘要 */
    private String analyzeDataSummary;

    /** 数据分析结果 */
    private String analyzeDataResult;

    /** 图表配置 */
    private Map<String, Object> chartConfig;

    /** 报告类型 */
    private String reportType;

    /** 生成的报告内容 */
    private String report;

    /** 输入令牌计数 */
    private AtomicInteger inputTokens = new AtomicInteger(0);

    /** 输出令牌计数 */
    private AtomicInteger outputTokens = new AtomicInteger(0);

    /** 工具调用结果列表 */
    private List<Map<String, Object>> toolCallResults;

    /** 上一轮的思考内容，用于连贯推理 */
    private String previousThinking;

    /** 示例 SQL（问题-SQL 对列表） */
    private List<Map<String, String>> sampleSqls;

    /** 历史问题列表（按时间升序排列） */
    private List<String> historicalQuestions;

    /** 是否终止对话 */
    private Boolean terminated = false;

    /** 临时文件路径（存储超阈值查询结果） */
    private List<String> queryResultFilePaths = new ArrayList<>();

    /** 连续工具调用计数 */
    private Integer consecutiveToolCalls = 0;

    /** 上一次执行的工具名称（用于检测同一工具重复调用） */
    private String lastToolCallName;

    /** 对话配置 */
    private ChatConfigDto chatConfig;

    /** 是否已审核最终 SQL 语句 */
    private Boolean finalSqlReviewed = false;

    /** 是否最终 SQL 语句有效 */
    private Boolean finalSqlValid = false;

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
