package cn.opensrcdevelop.auth.client.support;

import cn.opensrcdevelop.auth.client.util.HttpUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;

public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    public CustomOAuth2UserService() {
        super.setRestOperations(HttpUtil.getRestTemplate());
    }

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) throws OAuth2AuthenticationException {
        // 1. 加载用户信息
        OAuth2User oAuth2User = super.loadUser(userRequest);

        OAuth2Context context = new OAuth2Context();
        // 2. 设置用户属性
        var userAttributes = new OAuth2UserAttributes(oAuth2User.getAttributes());
        // 2.1 自定义用户属性
        var userAttributesCustomizerList = SpringContextUtil.getBeans(OAuth2UserAttributesCustomizer.class);
        for (OAuth2UserAttributesCustomizer customizer : userAttributesCustomizerList) {
            customizer.customize(userAttributes);
        }
        context.setUserAttributes(userAttributes);

        // 3. 设置令牌信息
        context.setAccessToken(userRequest.getAccessToken());
        OAuth2ContextHolder.setContext(context);

        return oAuth2User;
    }
}
