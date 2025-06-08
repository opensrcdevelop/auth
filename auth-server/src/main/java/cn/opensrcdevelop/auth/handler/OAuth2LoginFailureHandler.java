package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.common.util.WebUtil;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
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

        String errorMsg = "authFailure";
        if (exception instanceof OAuth2AuthenticationException ex) {
            String errorCd = ex.getError().getErrorCode();
            if (AuthConstants.THIRD_ACCOUNT_ALREADY_EXISTS_ERROR_CODE.equals(errorCd)) {
                errorMsg = "bindingExists";
            }

            if (AuthConstants.USER_LOCKED_ERROR_CODE.equals(errorCd)) {
                errorMsg = "userLocked";
            }
        }

        // 向子页面发送 HTML 响应
        String htmlContent = """
                    <html>
                        <script>
                            window.opener.postMessage('%s', '%s');
                        </script>
                    </html>
                """;
        htmlContent = String.format(htmlContent, errorMsg, TenantHelper.getTenantConsoleUrl());
        WebUtil.sendHtmlResponse(response, htmlContent, HttpStatus.UNAUTHORIZED);
    }
}
