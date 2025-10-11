package cn.opensrcdevelop.auth.authentication.email;

import cn.opensrcdevelop.auth.handler.LoginFailureHandler;
import cn.opensrcdevelop.auth.handler.LoginSuccessHandler;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.servlet.util.matcher.PathPatternRequestMatcher;

import java.io.IOException;

/**
 * 邮箱验证码登录过滤器
 */
@Slf4j
public class EmailCodeAuthenticationFilter extends AbstractAuthenticationProcessingFilter {

    private static final String PARAMETER_EMAIL = "email";
    private static final String PARAMETER_CODE = "code";
    private static final PathPatternRequestMatcher REQUEST_MATCHER = PathPatternRequestMatcher.withDefaults().matcher(HttpMethod.POST, "/login/email");

    public EmailCodeAuthenticationFilter(AuthenticationManager authenticationManager) {
        super(REQUEST_MATCHER, authenticationManager);
        super.setAuthenticationSuccessHandler(new LoginSuccessHandler());
        super.setAuthenticationFailureHandler(new LoginFailureHandler());
        super.setSecurityContextRepository(new HttpSessionSecurityContextRepository());
    }

    @Override
    public Authentication attemptAuthentication(HttpServletRequest request, HttpServletResponse response) throws AuthenticationException, IOException, ServletException {
        if (!request.getMethod().equals("POST")) {
            throw new AuthenticationServiceException("Authentication method not supported: " + request.getMethod());
        }

        String email = request.getParameter(PARAMETER_EMAIL);
        String requestCode = request.getParameter(PARAMETER_CODE);
        EmailCodeAuthenticationToken authenticationToken = new EmailCodeAuthenticationToken(email, requestCode);
        return this.getAuthenticationManager().authenticate(authenticationToken);
    }
}
