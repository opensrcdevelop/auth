package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.ai.util.SseUtil;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import io.vavr.Tuple;
import io.vavr.Tuple3;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.mvc.method.annotation.SseEmitter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.Objects;

@Slf4j
@Component(AnalyzeDataTool.TOOL_NAME)
@RequiredArgsConstructor
public class AnalyzeDataTool implements MethodTool {

    public static final String TOOL_NAME = "analyze_data";

    private static final String ANALYZE_DATA_FILE_NAME = "analyze_data_%s";
    private static final String ANALYZE_DATA_FILE_EXT = ".json";

    private final AnalyzeAgent analyzeAgent;
    private final ExecutePythonTool executePythonTool;

    @Value("${chatbi.max-python-execution-retry-count:3}")
    private Integer defaultMaxPythonExecutionRetryCount;

    @Tool(name = TOOL_NAME, description = "Used to analyze data and return the analysis results")
    @SuppressWarnings({"all"})
    public Response execute(@ToolParam(description = "The request to analyze data") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        SseEmitter emitter = chatContext.getEmitter();
        Response response = new Response();

        chatContext.setAnalyzeDataSummary(null);
        chatContext.setAnalyzeDataResult(null);
        File tempDataFile = null;
        try {
            // 1.1 检查是否存在查询数据
            if (CollectionUtils.isEmpty(ChatContextHolder.getChatContext().getQueryData())) {
                response.setSuccess(false);
                response.setError("The query data is empty, check the sql is executed");
                return response;
            }

            // 1.2 检查是否已审核 SQL
            if (!Boolean.TRUE.equals(chatContext.getFinalSqlReviewed())) {
                response.setSuccess(false);
                response.setError("The generated final SQL is not reviewed, please call tool review_sql first.");
                return response;
            }

            // 1.3 检查是否已生成有效 SQL
            if (!Boolean.TRUE.equals(chatContext.getFinalSqlValid())) {
                response.setSuccess(false);
                response.setError(
                        "The generated final SQL is not valid, please call tool generate_execute_sql to regenerate.");
                return response;
            }

            // 2. 创建临时数据文件
            tempDataFile = File.createTempFile(ANALYZE_DATA_FILE_NAME.formatted(System.currentTimeMillis()),
                    ANALYZE_DATA_FILE_EXT);
            try (FileWriter writer = new FileWriter(tempDataFile)) {
                writer.write(CommonUtil.serializeObject(ChatContextHolder.getChatContext().getQueryData()));
            }

            // 3. 生成 Python 数据分析代码
            SseUtil.sendChatBIToolCall(emitter, "开始生成用于分析数据的 Python 代码");
            Map<String, Object> pythonCodeResult = analyzeAgent.generatePythonCode(
                    ChatContextHolder.getChatContext().getChatClient(), tempDataFile.getAbsolutePath(),
                    request.generatePythonCodeInstruction);
            if (!Boolean.TRUE.equals(pythonCodeResult.get("success"))) {
                SseUtil.sendChatBIToolCall(emitter, "生成用于分析数据的 Python 代码失败");

                response.setSuccess(false);
                response.setError("Failed to generate Python code to analyze data, reason: %s"
                        .formatted(pythonCodeResult.get("error")));
                return response;
            }
            SseUtil.sendChatBIToolCall(emitter, "生成用于分析数据的 Python 代码成功");

            // 4. 执行 Python 数据分析代码
            SseUtil.sendChatBIToolCall(emitter, "开始执行用于分析数据的 Python 代码");
            Tuple3<Boolean, String, String> executeResult = executePythonCodeWithFix(
                    ChatContextHolder.getChatContext().getChatClient(),
                    tempDataFile.getAbsolutePath(),
                    (String) pythonCodeResult.get("python_code"),
                    (List<String>) pythonCodeResult.get("packages"),
                    getMaxPythonExecutionRetryCount(),
                    request.generatePythonCodeInstruction,
                    emitter);
            if (!Boolean.TRUE.equals(executeResult._1)) {
                SseUtil.sendChatBIToolCall(emitter, "执行用于分析数据的 Python 代码失败");

                response.setSuccess(false);
                response.setError(
                        "Failed to execute Python code to analyze data, reason: %s".formatted(executeResult._2));
                return response;
            }
            SseUtil.sendChatBIToolCall(emitter, "执行用于分析数据的 Python 代码成功");

            // 5. 处理 Python 数据分析代码执行结果
            SseUtil.sendChatBIToolCall(emitter, "开始分析 Python 代码执行结果和数据");
            Map<String, Object> analyzeResult = analyzeAgent.analyzeData(
                    ChatContextHolder.getChatContext().getChatClient(),
                    executeResult._2,
                    request.analyzeDataInstruction);
            if (!Boolean.TRUE.equals(analyzeResult.get("success"))) {
                SseUtil.sendChatBIToolCall(emitter, "分析 Python 代码执行结果和数据失败");

                response.setSuccess(false);
                response.setError("无法分析数据，原因：%s".formatted(analyzeResult.get("error")));
                return response;
            }
            SseUtil.sendChatBIToolCall(emitter, "分析 Python 代码执行结果和数据成功");

            String summary = (String) analyzeResult.get("summary");

            chatContext.setAnalyzeDataSummary(summary);
            chatContext.setAnalyzeDataResult(executeResult._2);

            response.setSuccess(true);
            response.setAnalysisResult(executeResult._2);
            response.setAnalysisSummary(summary);

            return response;
        } catch (Exception e) {
            throw new ServerException(e);
        } finally {
            try {
                if (Objects.nonNull(tempDataFile)) {
                    Files.deleteIfExists(tempDataFile.toPath());
                }
            } catch (IOException e) {
                log.error("删除临时数据文件 {} 失败", tempDataFile.getAbsolutePath(), e);
            }
        }
    }

    @Override
    public String toolName() {
        return TOOL_NAME;
    }

    @SuppressWarnings("all")
    private Tuple3<Boolean, String, String> executePythonCodeWithFix(ChatClient chatClient,
            String dataFilePath,
            String pythonCode,
            List<String> packages,
            int maxAttempts,
            String instruction,
            SseEmitter emitter) {
        int attempt = 0;
        String executeOutput = "";
        while (attempt <= maxAttempts) {
            attempt++;
            ExecutePythonTool.Request request = new ExecutePythonTool.Request();
            request.setScript(pythonCode);
            request.setPackages(packages);

            ExecutePythonTool.Response response = executePythonTool.execute(request);
            if (!Boolean.TRUE.equals(response.getSuccess())) {
                log.error("第 {} 次执行 Python 代码失败", attempt);
                SseUtil.sendChatBIToolCall(emitter, "第 %d 次执行 Python 代码失败，尝试修复".formatted(attempt));
                try {
                    Map<String, Object> fixResult = analyzeAgent.fixPythonCode(
                            chatClient,
                            dataFilePath,
                            pythonCode,
                            response.getResult(),
                            instruction);
                    if (!Boolean.TRUE.equals(fixResult.get("success"))) {
                        SseUtil.sendChatBIToolCall(emitter, "修复 Python 代码失败");
                        return Tuple.of(false, response.getResult(), pythonCode);
                    }
                    pythonCode = (String) fixResult.get("fixed_python_code");
                    packages = (List<String>) fixResult.get("packages");
                } catch (Exception e) {
                    log.error("修复 Python 代码失败", e);
                    SseUtil.sendChatBIToolCall(emitter, "修复 Python 代码失败");
                    return Tuple.of(false, response.getResult(), pythonCode);
                }
                SseUtil.sendChatBIToolCall(emitter, "修复 Python 代码成功");
            } else {
                executeOutput = response.getResult();
                break;
            }
        }

        return Tuple.of(true, executeOutput, pythonCode);
    }

    /**
     * 获取最大 Python 执行重试次数
     *
     * @return 最大 Python 执行重试次数
     */
    private int getMaxPythonExecutionRetryCount() {
        var chatConfig = ChatContextHolder.getChatContext().getChatConfig();
        return Objects.nonNull(chatConfig) && Objects.nonNull(chatConfig.getMaxPythonExecutionRetryCount())
                ? chatConfig.getMaxPythonExecutionRetryCount()
                : defaultMaxPythonExecutionRetryCount;
    }

    @Data
    public static class Request {
        @ToolParam(description = "The instruction to analyze data")
        @NotBlank
        private String analyzeDataInstruction;

        @ToolParam(description = "The instruction to generate Python code to analyze data")
        @NotBlank
        private String generatePythonCodeInstruction;
    }

    @Data
    public static class Response {
        private Boolean success;

        private String analysisSummary;

        private String analysisResult;

        private String error;
    }
}
