package cn.opensrcdevelop.auth.interceptor;

import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.filter.TraceFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.MDC;
import org.springframework.lang.NonNull;
import org.springframework.web.servlet.HandlerInterceptor;

public class TraceUserInterceptor implements HandlerInterceptor {

    @Override
    public boolean preHandle(@NonNull HttpServletRequest request, @NonNull HttpServletResponse response,
            @NonNull Object handler) throws Exception {
        // 获取当前用户名，设置到 MDC 中
        AuthUtil.getCurrentUsername().ifPresent(username -> {
            MDC.put(CommonConstants.MDC_USERNAME, username);
            TraceFilter.TTL_MDC.get().put(CommonConstants.MDC_USERNAME, username);
        });
        return true;
    }
}
