package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.tenant.support.TenantContext;
import cn.opensrcdevelop.tenant.support.TenantContextHolder;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.DefaultRedirectStrategy;
import org.springframework.security.web.RedirectStrategy;
import org.springframework.security.web.authentication.LoginUrlAuthenticationEntryPoint;
import org.springframework.security.web.util.UrlUtils;

import java.io.IOException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

@Slf4j
public class LoginTargetAuthenticationEntryPoint extends LoginUrlAuthenticationEntryPoint {

    private final RedirectStrategy redirectStrategy = new DefaultRedirectStrategy();

    public LoginTargetAuthenticationEntryPoint(String loginFormUrl) {
        super(loginFormUrl);
    }

    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException, ServletException {
        String loginFormUrl = determineUrlToUseForThisRequest(request, response, authException);

        // 添加租户二级域名
        TenantContext tenantContext = TenantContextHolder.getTenantContext();
        if (!tenantContext.isDefaultTenant()) {
            URL tmpUrl = new URL(loginFormUrl);
            loginFormUrl = tmpUrl.getProtocol() +
                    "://" +
                    tenantContext.getTenantCode() +
                    "." +
                    tmpUrl.getAuthority() +
                    tmpUrl.getPath();
        }

        // 非绝对路径
        if (!UrlUtils.isAbsoluteUrl(loginFormUrl)) {
            super.commence(request, response, authException);
            return;
        }

        String requestUrl =  request.getRequestURL().toString();

        if (StringUtils.isNotEmpty(request.getQueryString())) {
            requestUrl = requestUrl
                    .concat("?")
                    .concat(request.getQueryString());
        }
        String targetUrl = URLEncoder.encode(requestUrl, StandardCharsets.UTF_8);
        String redirectUrl = loginFormUrl
                                .concat("?target=")
                                .concat(targetUrl);
        log.debug("redirect to login url: {}", redirectUrl);
        redirectStrategy.sendRedirect(request, response, redirectUrl);
    }
}
