package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.user.LoginLogService;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import cn.opensrcdevelop.tenant.support.TenantHelper;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;

import java.io.IOException;
import java.util.Objects;

/**
 * OAuth2 登录成功处理
 *
 */
public class OAuth2LoginSuccessHandler implements AuthenticationSuccessHandler {

    private final LoginLogService loginLogService = SpringContextUtil.getBean(LoginLogService.class);

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {
        // 用户自主绑定，不保存登录日志
        HttpSession session = request.getSession(false);
        if (Objects.nonNull(session) && Objects.isNull(session.getAttribute(AuthConstants.SESSION_BIND_REQ_USER_ID))) {
            User user = (User) authentication.getPrincipal();
            setUserLoginInfo(user.getUserId());
        }

        // 向子页面发送 HTML 响应
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

    /**
     * 设置用户登录信息
     *
     * @param userId 用户 ID
     */
    private void setUserLoginInfo(String userId) {
        // 1. 保存登录日志
        loginLogService.saveLoginLog(userId);
    }
}
