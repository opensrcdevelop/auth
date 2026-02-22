package cn.opensrcdevelop.auth.audit.aop;

import cn.opensrcdevelop.auth.audit.annotation.Audit;
import cn.opensrcdevelop.auth.audit.context.AuditContext;
import cn.opensrcdevelop.auth.audit.entity.AuditLog;
import cn.opensrcdevelop.auth.audit.entity.ObjChangeLog;
import cn.opensrcdevelop.auth.audit.enums.AuditType;
import cn.opensrcdevelop.auth.audit.event.AuditEvent;
import cn.opensrcdevelop.common.exception.BizException;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.MessageUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.core.DefaultParameterNameDiscoverer;
import org.springframework.core.ParameterNameDiscoverer;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.ExpressionParser;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.expression.spel.support.StandardTypeLocator;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
@Aspect
public class AuditAspect {

    private static final ParameterNameDiscoverer PARAMETER_NAME_DISCOVERER = new DefaultParameterNameDiscoverer();
    private static final ExpressionParser EXPRESSION_PARSER = new SpelExpressionParser();
    private static final String SPEL_PREFIX = "{{";
    private static final String SPEL_SUFFIX = "}}";
    private static final String RESOURCE_TYPE_PACKAGE_PREFIX = "cn.opensrcdevelop.auth.audit.enums";
    private final MessageUtil messageUtil;

    @Pointcut("@annotation(cn.opensrcdevelop.auth.audit.annotation.Audit)")
    public void pointCut() {
    }

    @Around("pointCut()")
    public Object doAudit(ProceedingJoinPoint joinPoint) throws Throwable {
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        Audit auditAnnotation = methodSignature.getMethod().getAnnotation(Audit.class);
        boolean isSuccess = false;
        String exMsg = null;
        try {
            Object result = joinPoint.proceed();
            isSuccess = true;
            return result;
        } catch (Exception ex) {
            if (ex instanceof BizException bizException) {
                exMsg = messageUtil.getMsg(bizException.getMsgCode(), bizException.getParams());
            } else {
                exMsg = ex.getMessage();
            }
            throw ex;
        } finally {
            try {
                AuditEvent auditEvent = null;
                // 构建审计日志
                AuditLog auditLog = buildAuditLog(auditAnnotation, isSuccess, exMsg, joinPoint);
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

    private AuditLog buildAuditLog(Audit auditAnnotation, boolean isSuccess, String exMsg,
            ProceedingJoinPoint proceedingJoinPoint) {
        // SpEL 表达式解析上下文
        StandardEvaluationContext evaluationContext = getEvaluationContext(proceedingJoinPoint);

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
        auditLog.setRequestId(WebUtil.getRequestId());
        // 用户ID
        if (StringUtils.isNotEmpty(auditAnnotation.userId())) {
            auditLog.setUserId(parseSpel(evaluationContext, auditAnnotation.userId()));
        } else {
            auditLog.setUserId(SecurityContextHolder.getContext().getAuthentication().getName());
        }

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
        auditLog.setOperationDetail(isSuccess
                ? replaceExpressions(auditAnnotation.success(), evaluationContext)
                : replaceExpressions(auditAnnotation.fail(), evaluationContext));
        // 额外信息
        String extraInfo = auditAnnotation.extra();
        if (StringUtils.isNotEmpty(exMsg)) {
            if (StringUtils.isNotEmpty(extraInfo)) {
                extraInfo = extraInfo + "\n";
            }
            extraInfo = extraInfo + "Error：" + exMsg;
        }
        auditLog.setExtraInfo(extraInfo);
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

    private String replaceExpressions(String template, StandardEvaluationContext evaluationContext) {
        StringBuilder result = new StringBuilder();
        int lastIndex = 0;
        int startIndex;
        int endIndex;

        while ((startIndex = template.indexOf(SPEL_PREFIX, lastIndex)) != -1) {
            endIndex = template.indexOf(SPEL_SUFFIX, startIndex);
            if (endIndex == -1)
                break;

            // 添加非表达式部分
            result.append(template, lastIndex, startIndex);

            // 解析并添加表达式结果
            String expression = template.substring(startIndex + SPEL_PREFIX.length(), endIndex).trim();
            Object value = EXPRESSION_PARSER.parseExpression(expression).getValue(evaluationContext);
            result.append(value != null ? value.toString() : StringUtils.EMPTY);

            lastIndex = endIndex + SPEL_SUFFIX.length();
        }

        // 添加剩余部分
        result.append(template.substring(lastIndex));
        return result.toString();
    }

    private String parseSpel(EvaluationContext evaluationContext, String spel) {
        if (StringUtils.isBlank(spel)) {
            return spel;
        }

        // 解析 SpEL 表达式
        return EXPRESSION_PARSER.parseExpression(spel).getValue(evaluationContext, String.class);
    }

    private StandardEvaluationContext getEvaluationContext(ProceedingJoinPoint proceedingJoinPoint) {
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
        typeLocator.registerImport(RESOURCE_TYPE_PACKAGE_PREFIX);
        evaluationContext.setTypeLocator(typeLocator);
        evaluationContext.setBeanResolver(new BeanFactoryResolver(SpringContextUtil.getBeanFactory()));

        return evaluationContext;
    }
}
