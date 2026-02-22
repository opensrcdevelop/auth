package cn.opensrcdevelop.auth.client.authorize;

import java.util.Objects;
import java.util.function.Supplier;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.aop.framework.AopProxyUtils;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.expression.MethodBasedEvaluationContext;
import org.springframework.expression.EvaluationContext;
import org.springframework.security.access.expression.method.DefaultMethodSecurityExpressionHandler;
import org.springframework.security.access.expression.method.MethodSecurityExpressionOperations;
import org.springframework.security.core.Authentication;

public class AuthorizeExpressionHandler extends DefaultMethodSecurityExpressionHandler {

    @Override
    public EvaluationContext createEvaluationContext(Supplier<Authentication> authentication, MethodInvocation mi) {
        MethodBasedEvaluationContext context = new MethodBasedEvaluationContext(
                createCustomSecurityExpressionRoot(authentication, mi),
                AopUtils.getMostSpecificMethod(mi.getMethod(),
                        AopProxyUtils.ultimateTargetClass(Objects.requireNonNull(mi.getThis()))),
                mi.getArguments(),
                getParameterNameDiscoverer());
        context.setBeanResolver(getBeanResolver());
        return context;
    }

    @Override
    protected MethodSecurityExpressionOperations createSecurityExpressionRoot(Authentication authentication,
            MethodInvocation invocation) {
        return createCustomSecurityExpressionRoot(() -> authentication, invocation);
    }

    private MethodSecurityExpressionOperations createCustomSecurityExpressionRoot(
            Supplier<Authentication> authentication, MethodInvocation methodInvocation) {

        AuthorizeExpressionRootObject authorizeRootObject = new AuthorizeExpressionRootObject(authentication);
        authorizeRootObject.setMethodInvocation(methodInvocation);
        authorizeRootObject.setThis(methodInvocation.getThis());
        authorizeRootObject.setPermissionEvaluator(getPermissionEvaluator());
        authorizeRootObject.setTrustResolver(getTrustResolver());
        authorizeRootObject.setRoleHierarchy(getRoleHierarchy());
        authorizeRootObject.setDefaultRolePrefix(getDefaultRolePrefix());
        return authorizeRootObject;
    }
}
