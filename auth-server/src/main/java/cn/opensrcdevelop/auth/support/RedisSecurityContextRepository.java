package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.security.SupplierDeferredSecurityContext;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.RedisUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.context.DeferredSecurityContext;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextHolderStrategy;
import org.springframework.security.web.context.HttpRequestResponseHolder;
import org.springframework.security.web.context.SecurityContextRepository;
import org.springframework.stereotype.Component;

/**
 * 使用 Redis 存储 SecurityContext
 */
@Component
@RequiredArgsConstructor
public class RedisSecurityContextRepository implements SecurityContextRepository {

    private static final long EXPIRE_TIME = 5L;
    private final SecurityContextHolderStrategy securityContextHolderStrategy = SecurityContextHolder
            .getContextHolderStrategy();

    @Override
    @SuppressWarnings("all")
    public SecurityContext loadContext(HttpRequestResponseHolder requestResponseHolder) {
        throw new UnsupportedOperationException("Method deprecated.");
    }

    @Override
    public void saveContext(SecurityContext context, HttpServletRequest request, HttpServletResponse response) {
        String sessionId = getSessionId(request);
        if (StringUtils.isEmpty(sessionId)) {
            return;
        }

        SecurityContext emptyContext = securityContextHolderStrategy.createEmptyContext();
        String redisKey = getRedisKey(sessionId);
        // 当前 context 为空
        if (emptyContext.equals(context)) {
            RedisUtil.delete(redisKey);
        } else {
            // 存储认证信息
            RedisUtil.set(redisKey, context, EXPIRE_TIME, TimeUnit.MINUTES);
        }
    }

    @Override
    public boolean containsContext(HttpServletRequest request) {
        String sessionId = getSessionId(request);
        if (StringUtils.isEmpty(sessionId)) {
            return false;
        }
        return RedisUtil.hasKey(getRedisKey(sessionId));
    }

    @Override
    public DeferredSecurityContext loadDeferredContext(HttpServletRequest request) {
        Supplier<SecurityContext> supplier = () -> readSecurityContextFromRedis(request);
        return new SupplierDeferredSecurityContext(supplier, securityContextHolderStrategy);
    }

    private SecurityContext readSecurityContextFromRedis(HttpServletRequest request) {
        if (request == null) {
            return null;
        }
        String sessionId = getSessionId(request);
        if (StringUtils.isEmpty(sessionId)) {
            return null;
        }
        return RedisUtil.get(getRedisKey(sessionId), SecurityContext.class);
    }

    private String getSessionId(HttpServletRequest request) {
        String sessionId = request.getHeader(CommonConstants.SESSION_ID);
        if (StringUtils.isEmpty(sessionId)) {
            sessionId = request.getParameter(CommonConstants.SESSION_ID);
            HttpSession session = request.getSession(false);
            if (StringUtils.isEmpty(sessionId) && session != null) {
                sessionId = session.getId();
            }
        }
        return sessionId;
    }

    private String getRedisKey(String sessionId) {
        return AuthConstants.SECURITY_CONTEXT_REDIS_PREFIX + sessionId;
    }
}
