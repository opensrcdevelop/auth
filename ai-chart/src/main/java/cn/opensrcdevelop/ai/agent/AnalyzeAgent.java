package cn.opensrcdevelop.ai.agent;

import cn.opensrcdevelop.ai.model.ChartRecord;
import cn.opensrcdevelop.ai.prompt.Prompt;
import cn.opensrcdevelop.ai.prompt.PromptTemplate;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Map;

@Slf4j
@Component
@RequiredArgsConstructor
public class AnalyzeAgent {

    private final PromptTemplate promptTemplate;


    /**
     * 生成用户数据分析的 Python 代码
     *
     * @param chatClient ChatClient
     * @param chartRecord 临时图表记录
     * @param dataFilePath 数据文件路径
     * @return 生成的 Python 代码
     */
    public Map<String, Object> generatePythonCode(ChatClient chatClient, ChartRecord chartRecord, String dataFilePath) {
        // 1. 生成 Python 代码
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.GENERATE_PYTHON_CODE)
                .param("data_file_path", dataFilePath)
                .param("question", chartRecord.getQuestion())
                .param("sample_data", CommonUtil.serializeObject(chartRecord.getData().getFirst()))
                .param("column_aliases", CommonUtil.serializeObject(chartRecord.getColumns()));

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.GENERATE_PYTHON_CODE))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 分析图表数据
     *
     * @param chatClient  ChatClient
     * @param chartRecord 临时图表记录
     * @return 分析结果
     */
    public Map<String, Object> analyzeData(ChatClient chatClient, ChartRecord chartRecord, String pythonExecutionOutput) {
        // 1. 分析图表数据
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.ANALYZE_DATA)
                .param("question", chartRecord.getQuestion())
                .param("query_result", CommonUtil.serializeObject(chartRecord.getData()))
                .param("column_aliases", CommonUtil.serializeObject(chartRecord.getColumns()))
                .param("python_execution_output", pythonExecutionOutput);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.ANALYZE_DATA))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }

    /**
     * 生成分析报告
     *
     * @param chatClient      ChatClient
     * @param chartRecord     临时图表记录
     * @param analysisResults 分析结果
     * @return 分析报告
     */
    public Map<String, Object> generateAnalysisReport(ChatClient chatClient, ChartRecord chartRecord, String analysisResults, String analysisSummary) {
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

    /**
     * 修复 Python 代码
     *
     * @param chatClient ChatClient
     * @param chartRecord 临时图表记录
     * @param dataFilePath 数据文件路径
     * @param pythonCode Python 代码
     * @param pythonExecutionOutput Python 执行输出
     * @return 修复后的 Python 代码
     */
    public Map<String, Object> fixPythonCode(ChatClient chatClient,
                                             ChartRecord chartRecord,
                                             String dataFilePath,
                                             String pythonCode,
                                             String pythonExecutionOutput) {
        // 1. 修复 Python 代码
        Prompt prompt = promptTemplate.getTemplates().get(PromptTemplate.FIX_PYTHON_CODE)
                .param("question", chartRecord.getQuestion())
                .param("data_file_path", dataFilePath)
                .param("sample_data", CommonUtil.serializeObject(chartRecord.getData().getFirst()))
                .param("python_code", pythonCode)
                .param("error_output", pythonExecutionOutput);

        return chatClient.prompt()
                .system(prompt.buildSystemPrompt())
                .user(prompt.buildUserPrompt())
                .advisors(a -> a.param(PromptTemplate.PROMPT_TEMPLATE, PromptTemplate.FIX_PYTHON_CODE))
                .call()
                .entity(new ParameterizedTypeReference<Map<String, Object>>() {
                });
    }
}
