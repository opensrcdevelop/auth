package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.common.response.CodeEnum;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.InsufficientAuthenticationException;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2Error;
import org.springframework.security.oauth2.server.resource.InvalidBearerTokenException;
import org.springframework.security.web.AuthenticationEntryPoint;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * 资源服务器认证失败处理
 */
@Slf4j
public class ResourceAuthenticationExceptionHandler implements AuthenticationEntryPoint {

    private static final String ERROR = "error";
    private static final String ERROR_URI = "error_uri";
    private static final String ERROR_DESC = "error_description";

    @Override
    public void commence(HttpServletRequest request, HttpServletResponse response, AuthenticationException authException) throws IOException, ServletException {
        log.debug(authException.getMessage(), authException);
        Map<String, String> errorData = new LinkedHashMap<>();

        if (authException instanceof InsufficientAuthenticationException) {
            errorData.put(ERROR_DESC, "Not Authorized!");
            WebUtil.sendJsonResponse(R.optFailWithData(CodeEnum.RCD40001, errorData), HttpStatus.UNAUTHORIZED);
            return;
        }

        if (authException instanceof InvalidBearerTokenException e) {
            OAuth2Error oAuth2Error = e.getError();
            errorData.put(ERROR, oAuth2Error.getErrorCode());
            errorData.put(ERROR_DESC, oAuth2Error.getDescription());
            errorData.put(ERROR_URI, oAuth2Error.getUri());
            WebUtil.sendJsonResponse(R.optFailWithData(CodeEnum.RCD40001, errorData), HttpStatus.UNAUTHORIZED);
            return;
        }

        WebUtil.sendJsonResponse(R.optFail(CodeEnum.RCD40001), HttpStatus.UNAUTHORIZED);
    }
}
