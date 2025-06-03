package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.common.util.WebUtil;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;

import java.io.IOException;

/**
 * OAuth2 登录成功处理
 *
 */
@RequiredArgsConstructor
public class OAuth2LoginSuccessHandler implements AuthenticationSuccessHandler {

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {
        // 1. 向子页面发送 HTML 响应
        String htmlContent = """
                    <html>
                        <script>
                            window.opener.postMessage('authSuccess', '%s');
                        </script>
                    </html>
                """;
        htmlContent = String.format(htmlContent, TenantHelper.getTenantConsoleUrl());
        WebUtil.sendHtmlResponse(response, htmlContent, HttpStatus.OK);
    }
}
