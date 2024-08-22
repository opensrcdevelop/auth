package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;

import java.io.IOException;

/**
 * 登出成功处理
 */
public class AuthLogoutSuccessHandler implements LogoutSuccessHandler {

    @Override
    public void onLogoutSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException, ServletException {
        WebUtil.sendJsonResponse(R.ok(), HttpStatus.OK);
    }
}
