package cn.opensrcdevelop.auth.interceptor;

import cn.opensrcdevelop.auth.client.support.OAuth2Context;
import cn.opensrcdevelop.auth.client.support.OAuth2ContextHolder;
import cn.opensrcdevelop.auth.client.support.OAuth2Attributes;
import cn.opensrcdevelop.auth.client.support.OAuth2AttributesCustomizer;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.lang.NonNull;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.web.servlet.HandlerInterceptor;

import java.util.HashMap;

public class OAuth2ContextInterceptor implements HandlerInterceptor {

    @Override
    public boolean preHandle(@NonNull HttpServletRequest request, @NonNull HttpServletResponse response, @NonNull Object handler) throws Exception {
        OAuth2Context context =  OAuth2ContextHolder.getContext();
        // 1. 设置 context
        if (context == null) {
            String authorizeHeader = request.getHeader(CommonConstants.REQ_HEADER_AUTHORIZATION);
            if (StringUtils.isNotEmpty(authorizeHeader)) {
                context = new OAuth2Context();
                // 1.1 设置 access_token
                context.setAccessToken(new OAuth2AccessToken(OAuth2AccessToken.TokenType.BEARER, authorizeHeader.replace(CommonConstants.BEARER, StringUtils.EMPTY), null, null));

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
        }

        return true;
    }
}
