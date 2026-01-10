package cn.opensrcdevelop.auth.biz.util;

import cn.opensrcdevelop.common.util.HttpUtil;
import java.util.Map;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.context.expression.MapAccessor;
import org.springframework.expression.Expression;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

public class HttpExpressionUtil {

    private static final RestTemplate REST_TEMPLATE = new RestTemplateBuilder()
            .additionalInterceptors(new HttpUtil.CustomClientHttpRequestInterceptor()).build();
    private static final ExpressionParser PARSER = new SpelExpressionParser();
    private static final StandardEvaluationContext CONTEXT;

    static {
        CONTEXT = new StandardEvaluationContext(new HttpExpressionUtil());
    }

    public Object http(String url, String method, Map<String, String> headers, Object body,
            Class<?> responseType, String resultExpression) {

        // 构建请求头
        HttpHeaders httpHeaders = new HttpHeaders();
        if (headers != null) {
            headers.forEach(httpHeaders::add);
        }

        // 执行请求
        ResponseEntity<?> response = REST_TEMPLATE.exchange(
                url,
                HttpMethod.valueOf(method),
                body != null ? new HttpEntity<>(body, httpHeaders) : null,
                responseType);

        // 处理响应结果
        if (StringUtils.isBlank(resultExpression)) {
            return response.getBody();
        }

        // 使用 SpEL 表达式提取结果
        StandardEvaluationContext context = new StandardEvaluationContext(response.getBody());
        context.addPropertyAccessor(new MapAccessor());
        Expression exp = PARSER.parseExpression(resultExpression);
        return exp.getValue(context);
    }

    @SuppressWarnings("all")
    public static void parseSpELMap(Map<String, Object> spELMap) {
        if (MapUtils.isEmpty(spELMap)) {
            return;
        }

        for (Map.Entry<String, Object> entry : spELMap.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if (value instanceof String strVal && StringUtils.isNotBlank(strVal)) {
                int startIdx = strVal.indexOf("$$");
                int endIdx = strVal.lastIndexOf("$$");
                if (startIdx != -1 && endIdx != -1) {
                    String expression = strVal.substring(startIdx + 2, endIdx);
                    Expression exp = PARSER.parseExpression(expression);
                    spELMap.put(key, exp.getValue(CONTEXT));
                }
            }

            if (value instanceof Map mapVal) {
                parseSpELMap(mapVal);
            }
        }
    }
}
