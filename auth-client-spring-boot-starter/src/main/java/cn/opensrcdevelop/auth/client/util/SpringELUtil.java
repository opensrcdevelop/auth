package cn.opensrcdevelop.auth.client.util;

import cn.opensrcdevelop.auth.client.exception.AuthClientException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationContext;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.context.expression.MapAccessor;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;

import java.util.Map;

@Slf4j
public class SpringELUtil {

    private static final SpelExpressionParser spelExpressionParser = new SpelExpressionParser();

    private SpringELUtil() {}

    /**
     * 解析授权限制条件
     *
     * @param ctx 上下文
     * @param applicationContext 应用上下文
     * @param expression SpringEL 表达式
     * @return 解析结果
     */
    public static Boolean parseAuthorizeCondition(Map<String, Object> ctx, ApplicationContext applicationContext, String expression) {
        try {
            StandardEvaluationContext standardEvaluationContext = new StandardEvaluationContext(ctx);
            standardEvaluationContext.setBeanResolver(new BeanFactoryResolver(applicationContext.getAutowireCapableBeanFactory()));
            standardEvaluationContext.addPropertyAccessor(new MapAccessor());
            return spelExpressionParser.parseExpression(expression).getValue(standardEvaluationContext, Boolean.class);
        } catch (Exception e) {
            log.error("限制条件 SpringEL 表达式解析失败，expression: {}", expression);
            throw new AuthClientException(e, "限制条件 SpringEL 表达式解析失败。");
        }
    }
}
