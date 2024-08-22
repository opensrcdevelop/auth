package cn.opensrcdevelop.auth.authentication.password;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.OAuth2Error;
import org.springframework.security.oauth2.core.OAuth2ErrorCodes;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;
import org.springframework.security.web.authentication.AuthenticationConverter;
import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;

import java.util.*;

/**
 * 密码授权模式 token 转换器
 */
public class ResourceOwnerPasswordAuthenticationConverter implements AuthenticationConverter {
    private static final String DEFAULT_ERROR_URI = "https://datatracker.ietf.org/doc/html/rfc6749#section-4.1.2.1";

    @Override
    public Authentication convert(HttpServletRequest request) {
        String grantType = request.getParameter(OAuth2ParameterNames.GRANT_TYPE);
        if (!AuthConstants.GRANT_TYPE_PASSWORD.equals(grantType)) {
            return null;
        }

        Authentication clientPrincipal  = SecurityContextHolder.getContext().getAuthentication();
        MultiValueMap<String, String> parameters = WebUtil.getParameters(request);

        // scope (OPTIONAL)
        Set<String> scopes = null;
        String scope = parameters.getFirst(OAuth2ParameterNames.SCOPE);
        if (StringUtils.hasText(scope) && parameters.get(OAuth2ParameterNames.SCOPE).size() != 1) {
            throwError(OAuth2ErrorCodes.INVALID_SCOPE, OAuth2ParameterNames.SCOPE);
        }
        if (StringUtils.hasText(scope)) {
            scopes = new HashSet<>(Arrays.asList(StringUtils.delimitedListToStringArray(scope, " ")));
        }

        // username (REQUIRED)
        String username = parameters.getFirst(OAuth2ParameterNames.USERNAME);
        if (!StringUtils.hasText(username) || parameters.get(OAuth2ParameterNames.USERNAME).size() != 1) {
            throwError(OAuth2ErrorCodes.INVALID_REQUEST, OAuth2ParameterNames.USERNAME);
        }

        // password (REQUIRED)
        String password = parameters.getFirst(OAuth2ParameterNames.PASSWORD);
        if (!StringUtils.hasText(password) || parameters.get(OAuth2ParameterNames.PASSWORD).size() != 1) {
            throwError(OAuth2ErrorCodes.INVALID_REQUEST, OAuth2ParameterNames.PASSWORD);
        }

        Map<String, Object> additionalParameters = new HashMap<>();
        parameters.forEach((k, v) -> {
            if (!k.equals(OAuth2ParameterNames.GRANT_TYPE) &&
                    !k.equals(OAuth2ParameterNames.CLIENT_ID)) {
                additionalParameters.put(k, (v.size() == 1) ? v.get(0) : v.toArray(new String[0]));
            }
        });

        return new ResourceOwnerPasswordAuthenticationToken(new AuthorizationGrantType(AuthConstants.GRANT_TYPE_PASSWORD), clientPrincipal, scopes, additionalParameters);
    }

    private static void throwError(String errorCode, String parameterName) {
        OAuth2Error error = new OAuth2Error(errorCode, "OAuth 2.0 Parameter: " + parameterName, DEFAULT_ERROR_URI);
        throw new OAuth2AuthenticationException(error);
    }
}
