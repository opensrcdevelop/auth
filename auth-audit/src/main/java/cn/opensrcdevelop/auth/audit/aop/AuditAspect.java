package cn.opensrcdevelop.auth.audit.aop;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.audit.entity.ObjChangeLog;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.event.AuditEvent;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.filter.TraceFilter;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.ParameterNameDiscoverer;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.expression.spel.support.StandardTypeLocator;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

@Slf4j
@Component
@Aspect
public class AuditAspect implements ApplicationContextAware {

    private static final ParameterNameDiscoverer PARAMETER_NAME_DISCOVERER = new DefaultParameterNameDiscoverer();
    private static final ExpressionParser EXPRESSION_PARSER = new SpelExpressionParser();

    private ApplicationContext appCtx;

    @Pointcut("@annotation(cn.opensrcdevelop.auth.audit.annotation.Audit)")
    public void pointCut() {}

    @Around("pointCut()")
    public Object doAudit(ProceedingJoinPoint joinPoint) throws Throwable {
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        Audit auditAnnotation = methodSignature.getMethod().getAnnotation(Audit.class);
        boolean isSuccess = false;
        try {
            Object result = joinPoint.proceed();
            isSuccess = true;
            return result;
        } finally {
            try {
                AuditEvent auditEvent = null;
                // 构建审计日志
                AuditLog auditLog = buildAuditLog(auditAnnotation, isSuccess, joinPoint);
                auditEvent = new AuditEvent(auditLog, null);

                // 构建对象变化日志
                if (isSuccess) {
                    auditEvent = new AuditEvent(auditLog, buildObjChangeLogs());
                }

                // 发布审计事件
                SpringContextUtil.publishEvent(auditEvent);
            } catch (Exception e) {
                log.error("审计日志记录失败", e);
            } finally {
                // 清除审计上下文
                AuditContext.clearAuditContext();
            }
        }
    }

    private AuditLog buildAuditLog(Audit auditAnnotation, boolean isSuccess, ProceedingJoinPoint proceedingJoinPoint) {
        AuditLog auditLog = new AuditLog();
        auditLog.setAuditId(CommonUtil.getUUIDV7String());
        // 审计类型
        auditLog.setAuditType(auditAnnotation.type().ordinal());

        // 操作类型
        if (AuditType.SYS_OPERATION.equals(auditAnnotation.type())) {
            auditLog.setOperationType(auditAnnotation.sysOperation().ordinal());
        }
        if (AuditType.USER_OPERATION.equals(auditAnnotation.type())) {
            auditLog.setOperationType(auditAnnotation.userOperation().ordinal());
        }

        // 资源ID
        auditLog.setResourceId(auditAnnotation.resource().getId());
        // 请求ID
        auditLog.setRequestId(TraceFilter.TTL_MDC.get().get(CommonConstants.MDC_TRACE_ID));
        // 用户ID
        auditLog.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());

        String ip = WebUtil.getRemoteIP();
        // IP
        auditLog.setIp(ip);
        // IP归属地
        auditLog.setIpRegion(WebUtil.getIpRegion(ip));
        // 操作系统类型
        auditLog.setOsType(WebUtil.getDeviceOs());
        // 浏览器类型
        auditLog.setBrowserType(WebUtil.getBrowserType());
        // 设备类型
        auditLog.setDeviceType(WebUtil.getDeviceType());

        // 操作时间
        auditLog.setOperationTime(LocalDateTime.now());
        // 操作结果
        auditLog.setOperationResult(isSuccess);
        // 操作详情
        auditLog.setOperationDetail(isSuccess ? parseSpel(auditAnnotation.success(), proceedingJoinPoint) : parseSpel(auditAnnotation.error(), proceedingJoinPoint));
        // 额外信息
        auditLog.setExtraInfo(auditAnnotation.extra());
        return auditLog;
    }

    private List<ObjChangeLog> buildObjChangeLogs() {
        return CommonUtil.stream(AuditContext.getCompareObjList()).map(compareObj -> {
            ObjChangeLog objChangeLog = new ObjChangeLog();
            objChangeLog.setObjId(compareObj.getId());
            objChangeLog.setJavaType(compareObj.getJavaType());
            objChangeLog.setBefore(CommonUtil.serializeObjectAllowNull(compareObj.getBefore()));
            objChangeLog.setAfter(CommonUtil.serializeObjectAllowNull(compareObj.getAfter()));

            return objChangeLog;
        }).toList();
    }

    private String parseSpel(String spel, ProceedingJoinPoint proceedingJoinPoint) {
        if (StringUtils.isBlank(spel)) {
            return spel;
        }

        // 1. 添加上下文参数
        StandardEvaluationContext evaluationContext = new StandardEvaluationContext();
        // 1.1 添加方法参数
        MethodSignature methodSignature = (MethodSignature) proceedingJoinPoint.getSignature();
        Object[] args = proceedingJoinPoint.getArgs();
        String[] methodArgNames = PARAMETER_NAME_DISCOVERER.getParameterNames(methodSignature.getMethod());
        if (Objects.nonNull(args) && Objects.nonNull(methodArgNames) && args.length == methodArgNames.length) {
            for (int i = 0; i < args.length; i++) {
                evaluationContext.setVariable(methodArgNames[i], args[i]);
            }
        }

        // 1.2 添加自定义参数
        if (MapUtils.isNotEmpty(AuditContext.getSpelVariables())) {
            AuditContext.getSpelVariables().forEach(evaluationContext::setVariable);
        }

        // 1.3 添加 Link 解析参数
        StandardTypeLocator typeLocator = new StandardTypeLocator();
        typeLocator.registerImport("cn.opensrcdevelop.auth.audit.enums");
        evaluationContext.setTypeLocator(typeLocator);
        evaluationContext.setBeanResolver(new BeanFactoryResolver(appCtx.getAutowireCapableBeanFactory()));

        // 2. 解析 SpEL 表达式
        return EXPRESSION_PARSER.parseExpression(spel).getValue(evaluationContext, String.class);
    }

    @Override
    public void setApplicationContext(ApplicationContext appCtx) throws BeansException {
        this.appCtx = appCtx;
    }
}
