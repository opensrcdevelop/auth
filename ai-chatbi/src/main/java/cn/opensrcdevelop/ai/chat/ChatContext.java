package cn.opensrcdevelop.ai.chat;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import lombok.Data;
import org.springframework.ai.chat.client.ChatClient;

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

    /** 上一轮的思考内容，用于连贯推理 */
    private String previousThinking;

    /** 示例 SQL（问题-SQL 对列表） */
    private List<Map<String, String>> sampleSqls;

    private Boolean terminated = false;

    /** 等待用户输入的问题 */
    private Map<String, Object> pendingQuestion;

    /**
     * 设置等待用户输入
     *
     * @param question
     *            待回答的问题
     */
    public void setWaitingForUser(Map<String, Object> question) {
        this.pendingQuestion = question;
    }

    /**
     * 清除等待状态
     */
    public void clearWaitingState() {
        this.pendingQuestion = null;
    }

    /**
     * 是否在等待用户输入
     */
    public boolean isWaitingForUser() {
        return this.pendingQuestion != null;
    }

    /** 用户响应信号量 */
    private transient CountDownLatch userResponseLatch;

    /** 用户回答列表（支持多个问题） */
    private transient List<Map<String, Object>> userAnswers;

    /**
     * 等待用户回答
     * @param timeoutSeconds 超时时间（秒）
     * @return 用户回答列表，超时返回 null
     */
    public List<Map<String, Object>> waitForUserAnswer(long timeoutSeconds) {
        this.userResponseLatch = new CountDownLatch(1);
        try {
            boolean completed = userResponseLatch.await(timeoutSeconds, TimeUnit.SECONDS);
            if (completed) {
                return this.userAnswers;
            }
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
        return null;
    }

    /**
     * 添加用户回答并唤醒等待线程（累加而不是覆盖）
     */
    public void addUserAnswers(List<Map<String, Object>> answers) {
        if (this.userAnswers == null) {
            this.userAnswers = new ArrayList<>();
        }
        this.userAnswers.addAll(answers);
        if (this.userResponseLatch != null) {
            this.userResponseLatch.countDown();
        }
    }
}
