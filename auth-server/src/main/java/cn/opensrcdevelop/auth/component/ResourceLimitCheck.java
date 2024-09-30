package cn.opensrcdevelop.auth.component;

import cn.opensrcdevelop.auth.biz.annocation.ResourceLimit;
import cn.opensrcdevelop.auth.client.constants.AuthClientConstants;
import cn.opensrcdevelop.auth.client.support.ICheckAttribute;
import cn.opensrcdevelop.common.util.CommonUtil;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.expression.Expression;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.stereotype.Component;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.IntStream;

@Component("resourceLimitCheck")
public class ResourceLimitCheck implements ICheckAttribute {

    private final SpelExpressionParser expressionParser = new SpelExpressionParser();

    @Override
    public boolean check(Map<String, Object> attributes) {
        // 1. 获取方法调用
        MethodInvocation methodInvocation = (MethodInvocation) attributes.get(AuthClientConstants.METHOD_INVOCATION);

        // 2. 获取方法参数及参数名
        DefaultParameterNameDiscoverer parameterNameDiscoverer = new DefaultParameterNameDiscoverer();
        Object[] args = methodInvocation.getArguments();
        Method method = methodInvocation.getMethod();
        String[] paramNames = parameterNameDiscoverer.getParameterNames(method);
        if (Objects.nonNull(paramNames)) {
            ResourceLimit[] resourceLimits = method.getAnnotationsByType(ResourceLimit.class);
            return Arrays.stream(resourceLimits).allMatch(resourceLimit -> doCheck(resourceLimit, paramNames, args));
        }
        return true;
    }

    private boolean doCheck(ResourceLimit resourceLimit, String[] paramNames, Object[] args) {
        // 3. 解析 SpringEL 表达式获取请求资源的 ID
        StandardEvaluationContext evaluationContext = new StandardEvaluationContext();
        IntStream.range(0, paramNames.length).forEach(i -> evaluationContext.setVariable(paramNames[i], args[i]));
        List<String> limitIds = Arrays.asList(resourceLimit.ids());
        boolean isList = resourceLimit.isList();
        Expression expression = expressionParser.parseExpression(resourceLimit.idEl());
        // 4. 请求资源的 ID 在限制的资源 ID 范围内禁止访问
        if (isList) {
            @SuppressWarnings("unchecked")
            List<String> ids = expression.getValue(evaluationContext, List.class);
            return CommonUtil.stream(ids).noneMatch(limitIds::contains);
        } else {
            String id = expression.getValue(evaluationContext, String.class);
            return !limitIds.contains(id);
        }
    }
}
