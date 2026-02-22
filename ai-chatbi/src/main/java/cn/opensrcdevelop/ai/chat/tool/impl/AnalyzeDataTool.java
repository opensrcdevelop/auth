package cn.opensrcdevelop.ai.chat.tool.impl;

import cn.opensrcdevelop.ai.agent.AnalyzeAgent;
import cn.opensrcdevelop.ai.chat.ChatContext;
import cn.opensrcdevelop.ai.chat.ChatContextHolder;
import cn.opensrcdevelop.ai.chat.tool.MethodTool;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import io.vavr.Tuple;
import io.vavr.Tuple3;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.tool.annotation.Tool;
import org.springframework.ai.tool.annotation.ToolParam;
import org.springframework.stereotype.Component;

@Slf4j
@Component(AnalyzeDataTool.TOOL_NAME)
@RequiredArgsConstructor
public class AnalyzeDataTool implements MethodTool {

    public static final String TOOL_NAME = "analyze_data";

    private static final String ANALYZE_DATA_FILE_NAME = "analyze_data_%s";
    private static final String ANALYZE_DATA_FILE_EXT = ".json";

    private final AnalyzeAgent analyzeAgent;
    private final ExecutePythonTool executePythonTool;

    @Tool(name = TOOL_NAME, description = "Used to analyze data and return the analysis results")
    @SuppressWarnings({"all"})
    public Response execute(@ToolParam(description = "The request to analyze data") Request request) {
        ChatContext chatContext = ChatContextHolder.getChatContext();
        Response response = new Response();

        chatContext.setQuestion(request.getQuestion());
        chatContext.setAnalyzeDataSummary(null);
        chatContext.setAnalyzeDataResult(null);
        File tempDataFile = null;
        try {
            // 1. 创建临时数据文件
            tempDataFile = File.createTempFile(ANALYZE_DATA_FILE_NAME.formatted(System.currentTimeMillis()),
                    ANALYZE_DATA_FILE_EXT);
            try (FileWriter writer = new FileWriter(tempDataFile)) {
                writer.write(CommonUtil.serializeObject(ChatContextHolder.getChatContext().getQueryData()));
            }

            // 2. 生成 Python 数据分析代码
            Map<String, Object> pythonCodeResult = analyzeAgent.generatePythonCode(
                    ChatContextHolder.getChatContext().getChatClient(), tempDataFile.getAbsolutePath(),
                    request.generatePythonCodeInstruction);
            if (!Boolean.TRUE.equals(pythonCodeResult.get("success"))) {
                response.setSuccess(false);
                response.setError("无法生成用于分析数据的 Python 代码，原因：%s".formatted(pythonCodeResult.get("error")));
                return response;
            }

            // 3. 执行 Python 数据分析代码
            Tuple3<Boolean, String, String> executeResult = executePythonCodeWithFix(
                    ChatContextHolder.getChatContext().getChatClient(),
                    tempDataFile.getAbsolutePath(),
                    (String) pythonCodeResult.get("python_code"),
                    (List<String>) pythonCodeResult.get("packages"),
                    3,
                    request.fixGeneratePythonCodeInstruction);
            if (!Boolean.TRUE.equals(executeResult._1)) {
                response.setSuccess(false);
                response.setError("无法执行 Python 代码，原因：%s".formatted(executeResult._2));
                return response;
            }

            // 4. 处理 Python 数据分析代码执行结果
            Map<String, Object> analyzeResult = analyzeAgent.analyzeData(
                    ChatContextHolder.getChatContext().getChatClient(),
                    executeResult._2,
                    request.analyzeDataInstruction);
            if (!Boolean.TRUE.equals(analyzeResult.get("success"))) {
                response.setSuccess(false);
                response.setError("无法分析数据，原因：%s".formatted(analyzeResult.get("error")));
                return response;
            }

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
            String instruction) {
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
                Map<String, Object> fixResult = analyzeAgent.fixPythonCode(
                        chatClient,
                        dataFilePath,
                        pythonCode,
                        response.getResult(),
                        instruction);
                if (!Boolean.TRUE.equals(fixResult.get("success"))) {
                    return Tuple.of(false, response.getResult(), pythonCode);
                }
                pythonCode = (String) fixResult.get("fixed_python_code");
                packages = (List<String>) fixResult.get("packages");
            } else {
                executeOutput = response.getResult();
                break;
            }
        }

        return Tuple.of(true, executeOutput, pythonCode);
    }

    @Data
    public static class Request {

        @ToolParam(description = "The question to analyze data")
        private String question;

        @ToolParam(description = "The instruction to generate Python code used to analyze data", required = false)
        private String generatePythonCodeInstruction;

        @ToolParam(description = "The instruction to fix Python code used to analyze data", required = false)
        private String fixGeneratePythonCodeInstruction;

        @ToolParam(description = "The instruction to analyze data", required = false)
        private String analyzeDataInstruction;
    }

    @Data
    public static class Response {

        @ToolParam(description = "The success of analyze data")
        private Boolean success;

        @ToolParam(description = "The summary of analyze data")
        private String analysisSummary;

        @ToolParam(description = "The result of analyze data")
        private String analysisResult;

        @ToolParam(description = "The error message if analyze data failed")
        private String error;
    }
}
