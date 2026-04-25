package cn.opensrcdevelop.auth.interceptor;

import cn.opensrcdevelop.auth.annoation.OpenApi;
import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.entity.client.Client;
import cn.opensrcdevelop.auth.biz.service.client.ClientService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.annoation.RestResponse;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.Optional;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.springframework.lang.NonNull;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.web.method.HandlerMethod;
import org.springframework.web.servlet.HandlerInterceptor;

@RequiredArgsConstructor
public class OpenApiInterceptor implements HandlerInterceptor {

    private final SystemSettingService systemSettingService;
    private final ClientService clientService;

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

                // 2.1 存储控制台客户端 ID
                if (StringUtils.isEmpty(consoleClientId)) {
                    Client client = clientService
                            .getOne(Wrappers.<Client>lambdaQuery().eq(Client::getClientName,
                                    CommonConstants.CONSOLE_CLIENT_NAME));
                    if (Objects.nonNull(client)) {
                        String clientId = client.getClientId();
                        systemSettingService.saveSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_ID, clientId);
                        consoleClientId = clientId;
                    }
                }

                if (!Strings.CS.equals(consoleClientId, clientIdOp.get())) {
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
