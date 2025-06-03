package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.common.util.WebUtil;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AuthenticationFailureHandler;

import java.io.IOException;

/**
 * OAuth2 登录失败处理
 *
 */
@Slf4j
@RequiredArgsConstructor
public class OAuth2LoginFailureHandler implements AuthenticationFailureHandler {

    @Override
    public void onAuthenticationFailure(HttpServletRequest request, HttpServletResponse response, AuthenticationException exception) throws IOException, ServletException {
        log.error(exception.getMessage(), exception);

        // 1. 向子页面发送 HTML 响应
        String htmlContent = """
                    <html>
                        <script>
                            window.opener.postMessage('authFailure', '%s');
                        </script>
                    </html>
                """;
        htmlContent = String.format(htmlContent, TenantHelper.getTenantConsoleUrl());
        WebUtil.sendHtmlResponse(response, htmlContent, HttpStatus.UNAUTHORIZED);
    }
}
