package cn.opensrcdevelop.ai.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Text2SqlUtil {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    private Text2SqlUtil() {}

    /**
     * 提取 SQL 语句
     *
     * @param llmResponse LLM 响应
     * @return SQL 语句
     */
    public static String extractSql(String llmResponse) {
        if (StringUtils.isEmpty(llmResponse)) {
            return llmResponse;
        }

        // 1. 提取最后一个以 WITH 开头、以分号结尾的 CTE 语句
        Pattern withPattern = Pattern.compile("\\bWITH\\b .*?;", Pattern.DOTALL);
        Matcher withMatcher = withPattern.matcher(llmResponse);
        String lastWith = null;
        while (withMatcher.find()) {
            lastWith = withMatcher.group();
        }
        if (lastWith != null) {
            return lastWith;
        }

        // 2. 提取最后一个 SELECT...; 形式的语句（非 markdown）
        Pattern selectPattern = Pattern.compile("SELECT.*?;", Pattern.DOTALL);
        Matcher selectMatcher = selectPattern.matcher(llmResponse);
        String lastSelect = null;
        while (selectMatcher.find()) {
            lastSelect = selectMatcher.group();
        }
        if (lastSelect != null) {
            return lastSelect;
        }

        // 3. 提取 ```sql ... ``` 中的最后一段
        Pattern sqlBlockPattern = Pattern.compile("```sql\\n(.*?)```", Pattern.DOTALL);
        Matcher sqlBlockMatcher = sqlBlockPattern.matcher(llmResponse);
        String lastSqlBlock = null;
        while (sqlBlockMatcher.find()) {
            lastSqlBlock = sqlBlockMatcher.group(1);
        }
        if (lastSqlBlock != null) {
            return lastSqlBlock;
        }

        // 4. 提取任意 ``` ... ``` 中的最后一段
        Pattern genericBlockPattern = Pattern.compile("```(.*?)```", Pattern.DOTALL);
        Matcher genericBlockMatcher = genericBlockPattern.matcher(llmResponse);
        String lastGenericBlock = null;
        while (genericBlockMatcher.find()) {
            lastGenericBlock = genericBlockMatcher.group(1);
        }
        return Objects.requireNonNullElse(lastGenericBlock, llmResponse);
    }

    /**
     * 提取 JSON 字符串
     *
     * @param llmResponse LLM 响应
     * @return JSON 字符串
     */
    public static String extractJson(String llmResponse) {
        if (StringUtils.isEmpty(llmResponse)) {
            return null;
        }

        // 1. 先尝试直接解析整段
        try {
            JsonNode node = MAPPER.readTree(llmResponse.trim());
            return node.toString();
        } catch (Exception ignored) {
            // 不是纯 JSON，继续下一步
        }

        // 2. 提取 ```json ... ``` 中的最后一段
        Pattern jsonBlockPattern = Pattern.compile("```json\\s*(\\{.*?\\}|\\[.*?\\])\\s*```", Pattern.DOTALL);
        Matcher jsonBlockMatcher = jsonBlockPattern.matcher(llmResponse);
        String lastJsonBlock = null;
        while (jsonBlockMatcher.find()) {
            lastJsonBlock = jsonBlockMatcher.group(1);
        }
        if (lastJsonBlock != null && isValidJson(lastJsonBlock)) {
            return lastJsonBlock;
        }

        // 3. 提取任意 ``` ... ``` 中的最后一段
        Pattern genericBlockPattern = Pattern.compile("```(\\{.*?\\}|\\[.*?\\])```", Pattern.DOTALL);
        Matcher genericBlockMatcher = genericBlockPattern.matcher(llmResponse);
        String lastGenericBlock = null;
        while (genericBlockMatcher.find()) {
            lastGenericBlock = genericBlockMatcher.group(1);
        }
        if (lastGenericBlock != null && isValidJson(lastGenericBlock)) {
            return lastGenericBlock;
        }

        // 4. 在整个文本中找最后一个合法 JSON 对象 / 数组
        Pattern loosePattern = Pattern.compile("(\\{[^{}]*\\}|\\[[^\\[\\]]*\\])", Pattern.DOTALL);
        Matcher looseMatcher = loosePattern.matcher(llmResponse);
        String lastLoose = null;
        while (looseMatcher.find()) {
            String candidate = looseMatcher.group(1);
            if (isValidJson(candidate)) {
                lastLoose = candidate;
            }
        }
        return lastLoose;
    }

    private static boolean isValidJson(String candidate) {
        try {
            MAPPER.readTree(candidate);
            return true;
        } catch (Exception e) {
            return false;
        }
    }
}
