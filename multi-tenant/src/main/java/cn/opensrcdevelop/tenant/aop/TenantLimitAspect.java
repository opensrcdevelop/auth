package cn.opensrcdevelop.tenant.aop;

import cn.opensrcdevelop.tenant.annoation.TenantLimit;
import cn.opensrcdevelop.tenant.support.TenantContext;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;

import java.text.MessageFormat;
import java.util.Arrays;
import java.util.List;

@Component
@Aspect
public class TenantLimitAspect {

    private static final String ACCESS_DENIED_EXCEPTION_MESSAGE = "租户 {0} 不在允许访问的租户 {1} 内";

    @Pointcut("@annotation(cn.opensrcdevelop.tenant.annoation.TenantLimit)")
    public void pointCut() {}

    @Before("pointCut()")
    public void before(JoinPoint joinPoint) {
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        TenantLimit tenantLimit = methodSignature.getMethod().getAnnotation(TenantLimit.class);
        List<String> limitedTenants = Arrays.asList(tenantLimit.value());
        String currentTenant = TenantContext.getTenant();
        if (!limitedTenants.contains(currentTenant)) {
            throw new AccessDeniedException(MessageFormat.format(ACCESS_DENIED_EXCEPTION_MESSAGE, currentTenant, limitedTenants));
        }
    }
}
