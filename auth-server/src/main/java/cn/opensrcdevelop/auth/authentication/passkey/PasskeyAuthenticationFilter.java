package cn.opensrcdevelop.auth.authentication.passkey;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.handler.LoginFailureHandler;
import cn.opensrcdevelop.auth.handler.LoginSuccessHandler;
import cn.opensrcdevelop.common.config.AuthorizationServerProperties;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.web.authentication.AbstractAuthenticationProcessingFilter;
import org.springframework.security.web.context.HttpSessionSecurityContextRepository;
import org.springframework.security.web.servlet.util.matcher.PathPatternRequestMatcher;

/**
 * Passkey 登录过滤器 用户只需点击 Passkey 登录按钮，无需输入用户名
 */
@Slf4j
public class PasskeyAuthenticationFilter extends AbstractAuthenticationProcessingFilter {

    private static final String PARAMETER_CREDENTIAL_ID = "credentialId";
    private static final String PARAMETER_RESPONSE = "response";
    private static final String PARAMETER_CLIENT_DATA_JSON = "clientDataJSON";
    private static final String PARAMETER_SIGNATURE = "signature";

    private static final PathPatternRequestMatcher REQUEST_MATCHER = PathPatternRequestMatcher.withDefaults().matcher(
            HttpMethod.POST,
            SpringContextUtil.getBean(AuthorizationServerProperties.class).getApiPrefix()
                    .concat(AuthConstants.PASSKEY_LOGIN_URL));

    public PasskeyAuthenticationFilter(AuthenticationManager authenticationManager) {
        super(REQUEST_MATCHER, authenticationManager);
        super.setAuthenticationSuccessHandler(new LoginSuccessHandler());
        super.setAuthenticationFailureHandler(new LoginFailureHandler());
        super.setSecurityContextRepository(new HttpSessionSecurityContextRepository());
    }

    @Override
    public Authentication attemptAuthentication(HttpServletRequest request, HttpServletResponse response)
            throws AuthenticationException, IOException, ServletException {
        log.info("Passkey 登录请求: {}", request.getRequestURI());

        if (!request.getMethod().equals("POST")) {
            throw new AuthenticationServiceException("Authentication method not supported: " + request.getMethod());
        }

        String credentialId = request.getParameter(PARAMETER_CREDENTIAL_ID);
        String responseData = request.getParameter(PARAMETER_RESPONSE);
        String clientDataJSON = request.getParameter(PARAMETER_CLIENT_DATA_JSON);
        String signature = request.getParameter(PARAMETER_SIGNATURE);

        log.info("Passkey 登录参数: credentialId={}, response={}, clientDataJSON={}, signature={}",
                credentialId,
                responseData != null ? responseData.substring(0, Math.min(50, responseData.length())) + "..." : null,
                clientDataJSON != null
                        ? clientDataJSON.substring(0, Math.min(50, clientDataJSON.length())) + "..."
                        : null,
                signature != null ? signature.substring(0, Math.min(50, signature.length())) + "..." : null);

        // 验证必要参数
        if (credentialId == null || credentialId.isBlank()) {
            throw new AuthenticationServiceException("凭证ID不能为空");
        }
        if (responseData == null || responseData.isBlank()) {
            throw new AuthenticationServiceException("认证响应不能为空");
        }

        PasskeyAuthenticationToken authenticationToken = new PasskeyAuthenticationToken(
                credentialId, responseData, clientDataJSON, signature);
        return this.getAuthenticationManager().authenticate(authenticationToken);
    }
}
