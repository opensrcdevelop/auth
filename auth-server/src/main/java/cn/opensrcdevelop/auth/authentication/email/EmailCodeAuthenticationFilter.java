package cn.opensrcdevelop.auth.authentication.email;

import cn.opensrcdevelop.auth.biz.service.VerificationCodeService;
import cn.opensrcdevelop.auth.handler.LoginFailureHandler;
import cn.opensrcdevelop.auth.handler.LoginSuccessHandler;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.util.matcher.AntPathRequestMatcher;

import java.io.IOException;

/**
 * 邮箱验证码登录过滤器
 */
@Slf4j
public class EmailCodeAuthenticationFilter extends AbstractAuthenticationProcessingFilter {

    private static final String PARAMETER_EMAIL = "email";
    private static final String PARAMETER_CODE = "code";
    private static final AntPathRequestMatcher DEFAULT_ANT_PATH_REQUEST_MATCHER = new AntPathRequestMatcher("/login/email", "POST");
    private final VerificationCodeService verificationCodeService = SpringContextUtil.getBean(VerificationCodeService.class);

    public EmailCodeAuthenticationFilter(AuthenticationManager authenticationManager) {
        super(DEFAULT_ANT_PATH_REQUEST_MATCHER, authenticationManager);
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
        if (!verificationCodeService.verifyCode(requestCode)) {
            throw new AuthenticationServiceException("Invalid Code");
        }

        EmailCodeAuthenticationToken authenticationToken = new EmailCodeAuthenticationToken(email, requestCode, requestCode);
        return this.getAuthenticationManager().authenticate(authenticationToken);
    }
}
