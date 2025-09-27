package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.chat.tool.ExecutePythonTool;
import cn.opensrcdevelop.ai.model.ChartRecord;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class AnalyzeAgent {

    private static final String ANALYZE_DATA_FILE_NAME = "analyze_data_%s";
    private static final String ANALYZE_DATA_FILE_EXT = ".json";

    private final PromptTemplate promptTemplate;
    private final ExecutePythonTool executePythonTool;

    /**
     * 分析图表数据
     *
     * @param chatClient  ChatClient
     * @param chartRecord 临时图表记录
     * @return 分析结果
     */
    public Map<String, Object> analyzeData(ChatClient chatClient, ChartRecord chartRecord) {
        // 1. 将数据写入临时文件中
        File tempDataFile = null;
        try {
            tempDataFile = File.createTempFile(ANALYZE_DATA_FILE_NAME.formatted(System.currentTimeMillis()), ANALYZE_DATA_FILE_EXT);
            try (FileWriter writer = new FileWriter(tempDataFile)) {
                writer.write(CommonUtil.serializeObject(chartRecord.getData()));
            }
        } catch (Exception ex) {
            throw new ServerException("error writing analyze data file", ex);
        }

        try {
            // 1. 分析图表数据
            Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.ANALYZE_DATA)
                    .param("data_file_path", tempDataFile.getAbsoluteFile())
                    .param("question", chartRecord.getQuestion())
                    .param("sample_data", CommonUtil.serializeObject(chartRecord.getData().getFirst()))
                    .param("column_aliases", CommonUtil.serializeObject(chartRecord.getColumns()))
                    .param("execute_python_tool_name", executePythonTool.toolName())
                    .param("max_execution_attempts", 3);

            return chatClient.prompt()
                    .system(prompt.buildSystemPrompt())
                    .user(prompt.buildUserPrompt())
                    .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.ANALYZE_DATA))
                    .tools(executePythonTool)
                    .call()
                    .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                    });
        } finally {
            try {
                Files.deleteIfExists(tempDataFile.toPath());
            } catch (Exception ex) {
                log.error("delete temp data file error", ex);
            }
        }
    }

    /**
     * 生成分析报告
     *
     * @param chatClient      ChatClient
     * @param chartRecord     临时图表记录
     * @param analysisResults 分析结果
     * @return 分析报告
     */
    public Map<String, Object> generateAnalysisReport(ChatClient chatClient, ChartRecord chartRecord, List<Map<String, Object>> analysisResults, String analysisSummary) {
        // 1. 生成分析报告
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.GENERATE_REPORT)
                .param("question", chartRecord.getQuestion())
                .param("query_result", CommonUtil.serializeObject(chartRecord.getData()))
                .param("column_aliases", CommonUtil.serializeObject(chartRecord.getColumns()))
                .param("analysis_results", CommonUtil.serializeObject(analysisResults))
                .param("analysis_summary", analysisSummary)
                .param("current_time", LocalDateTime.now().format(DateTimeFormatter.ofPattern(CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSSSSS)));

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.GENERATE_REPORT))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }
}
