package cn.opensrcdevelop.ai.prompt;

import lombok.Getter;
import lombok.Setter;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@ConfigurationProperties("ai.prompt")
@Getter
@Setter
public class PromptTemplate {

    public static final String PROMPT_TEMPLATE = "prompt_template";
    public static final String SELECT_TABLE = "select_table";
    public static final String GENERATE_SQL = "generate_sql";
    public static final String GENERATE_CHART = "generate_chart";
    public static final String FIX_SQL = "fix_sql";
    public static final String ANALYZE_DATA = "analyze_data";
    public static final String GENERATE_REPORT = "generate_report";
    public static final String REWRITE_QUESTION = "rewrite_question";
    public static final String GENERATE_PYTHON_CODE = "generate_python_code";
    public static final String FIX_PYTHON_CODE = "fix_python_code";
    public static final String ANSWER_QUESTION = "answer_question";

    private Map<String, Prompt> templates;
}
