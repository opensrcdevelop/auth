package cn.opensrcdevelop.auth.interceptor;

import cn.opensrcdevelop.auth.annoation.OpenApi;
import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

@RequiredArgsConstructor
public class OpenApiInterceptor implements HandlerInterceptor {

    private final SystemSettingService systemSettingService;

    @Override
    public boolean preHandle(@NonNull HttpServletRequest request, @NonNull HttpServletResponse response,
            @NonNull Object handler) throws Exception {
        if (handler instanceof HandlerMethod handlerMethod && !isOpenApi(handlerMethod)) {
            // 1. 检查租户上下文是否存在
            if (Objects.isNull(TenantContextHolder.getTenantContext())) {
                // 1.1 再次设置租户上下文
                TenantContextHolder.setTenantContext(request);
            }

            // 2. 检查客户端是否为控制台客户端
            Optional<String> clientIdOp = AuthUtil.getCurrentClientId();
            if (clientIdOp.isPresent()) {
                String consoleClientId = systemSettingService.getSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_ID,
                        String.class);
                if (!StringUtils.equals(consoleClientId, clientIdOp.get())) {
                    throw new AccessDeniedException("Not console client");
                }
            }
        }
        return true;
    }

    private boolean isOpenApi(HandlerMethod handlerMethod) {
        Class<?> clazz = handlerMethod.getBeanType();
        Method method = handlerMethod.getMethod();
        return clazz.isAnnotationPresent(OpenApi.class) || method.isAnnotationPresent(RestResponse.class);
    }
}
