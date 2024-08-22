package cn.opensrcdevelop.auth.client.support;

import cn.opensrcdevelop.common.util.SpringContextUtil;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserRequest;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserService;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.oidc.user.OidcUser;

public class CustomOidcUserService extends OidcUserService {

    @Override
    public OidcUser loadUser(OidcUserRequest userRequest) throws OAuth2AuthenticationException {
        // 1. 加载用户信息
        OidcUser oidcUser = super.loadUser(userRequest);

        OAuth2Context context = new OAuth2Context();
        // 2. 设置用户属性
        var userAttributes = new OAuth2UserAttributes(oidcUser.getAttributes());
        // 2.1 自定义用户属性
        var userAttributesCustomizerList = SpringContextUtil.getBeans(OAuth2UserAttributesCustomizer.class);
        for (OAuth2UserAttributesCustomizer customizer : userAttributesCustomizerList) {
            customizer.customize(userAttributes);
        }
        context.setUserAttributes(userAttributes);

        // 3. 设置令牌信息
        context.setAccessToken(userRequest.getAccessToken());
        OAuth2ContextHolder.setContext(context);

        return oidcUser;
    }
}
