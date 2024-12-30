package cn.opensrcdevelop.auth.interceptor;

import cn.opensrcdevelop.auth.client.support.OAuth2Attributes;
import cn.opensrcdevelop.auth.client.support.OAuth2AttributesCustomizer;
import cn.opensrcdevelop.auth.client.support.OAuth2Context;
import cn.opensrcdevelop.auth.client.support.OAuth2ContextHolder;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.NonNull;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.server.resource.web.BearerTokenResolver;
import org.springframework.security.oauth2.server.resource.web.DefaultBearerTokenResolver;
import org.springframework.web.servlet.HandlerInterceptor;

import java.util.HashMap;

public class OAuth2ContextInterceptor implements HandlerInterceptor {

    @Override
    public boolean preHandle(@NonNull HttpServletRequest request, @NonNull HttpServletResponse response, @NonNull Object handler) throws Exception {
        OAuth2Context context =  OAuth2ContextHolder.getContext();
        BearerTokenResolver bearerTokenResolver = new DefaultBearerTokenResolver();
        String accessToken = bearerTokenResolver.resolve(request);
        // 1. 设置 context
        if (context == null) {
            if (StringUtils.isNotEmpty(accessToken)) {
                context = new OAuth2Context();
                // 1.1 设置 access_token
                context.setAccessToken(new OAuth2AccessToken(OAuth2AccessToken.TokenType.BEARER, accessToken, null, null));

                // 1.2 设置属性
                var attributes = new OAuth2Attributes(new HashMap<>());
                context.setOAuth2Attributes(attributes);
                // 1.2.1 自定义用户属性
                var userAttributesCustomizerList = SpringContextUtil.getBeans(OAuth2AttributesCustomizer.class);
                for (OAuth2AttributesCustomizer customizer : userAttributesCustomizerList) {
                    customizer.customize(attributes);
                }
                OAuth2ContextHolder.setContext(context);
            }
        } else {
            // 1.3 判断请求的 access_token 是否更新
            if (StringUtils.isNotEmpty(accessToken) && !StringUtils.equals(context.getAccessToken().getTokenValue(), accessToken)) {
                // 1.3.1 更新 access_token
                context.setAccessToken(new OAuth2AccessToken(OAuth2AccessToken.TokenType.BEARER, accessToken, null, null));
            }
        }

        return true;
    }
}
