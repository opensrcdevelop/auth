package cn.opensrcdevelop.auth.biz.component;

import lombok.Setter;
import org.springframework.core.convert.converter.Converter;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.endpoint.OAuth2AccessTokenResponse;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;

import java.util.*;

@Setter
public class CustomMapOAuth2AccessTokenResponseConverter implements Converter<Map<String, Object>, OAuth2AccessTokenResponse> {

    private String accessTokenAttrName = OAuth2ParameterNames.ACCESS_TOKEN;
    private String refreshTokenAttrName = OAuth2ParameterNames.REFRESH_TOKEN;
    private Map<String, Object> additionalParams = new HashMap<>();

    private static final Set<String> TOKEN_RESPONSE_PARAMETER_NAMES = new HashSet<>(
            Arrays.asList(OAuth2ParameterNames.ACCESS_TOKEN, OAuth2ParameterNames.EXPIRES_IN,
                    OAuth2ParameterNames.REFRESH_TOKEN, OAuth2ParameterNames.SCOPE, OAuth2ParameterNames.TOKEN_TYPE));

    @SuppressWarnings("all")
    @Override
    public OAuth2AccessTokenResponse convert(Map<String, Object> source) {
        String accessToken = getParameterValue(source, accessTokenAttrName);
        long expiresIn = getExpiresIn(source);
        String refreshToken = getParameterValue(source, refreshTokenAttrName);
        Map<String, Object> additionalParameters = new LinkedHashMap<>();
        for (Map.Entry<String, Object> entry : source.entrySet()) {
            if (!TOKEN_RESPONSE_PARAMETER_NAMES.contains(entry.getKey())) {
                additionalParameters.put(entry.getKey(), entry.getValue());
            }
        }

        if (!additionalParams.isEmpty()) {
            additionalParameters.putAll(additionalParams);
        }

        // @formatter:off
        var response = OAuth2AccessTokenResponse.withToken(accessToken)
                .tokenType(OAuth2AccessToken.TokenType.BEARER)
                .expiresIn(expiresIn)
                .refreshToken(refreshToken)
                .additionalParameters(additionalParameters)
                .build();
        // @formatter:on
        additionalParameters.clear();
        return response;
    }

    private static long getExpiresIn(Map<String, Object> tokenResponseParameters) {
        return getParameterValue(tokenResponseParameters, OAuth2ParameterNames.EXPIRES_IN, 0L);
    }

    private static String getParameterValue(Map<String, Object> tokenResponseParameters, String parameterName) {
        Object obj = tokenResponseParameters.get(parameterName);
        return (obj != null) ? obj.toString() : null;
    }

    @SuppressWarnings("all")
    private static long getParameterValue(Map<String, Object> tokenResponseParameters, String parameterName, long defaultValue) {
        long parameterValue = defaultValue;

        Object obj = tokenResponseParameters.get(parameterName);
        if (obj != null) {
            // Final classes Long and Integer do not need to be coerced
            if (obj.getClass() == Long.class) {
                parameterValue = (Long) obj;
            }
            else if (obj.getClass() == Integer.class) {
                parameterValue = (Integer) obj;
            }
            else {
                // Attempt to coerce to a long (typically from a String)
                try {
                    parameterValue = Long.parseLong(obj.toString());
                }
                catch (NumberFormatException ignored) {
                }
            }
        }

        return parameterValue;
    }
}
