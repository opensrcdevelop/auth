package cn.opensrcdevelop.auth.client.support;

import cn.opensrcdevelop.auth.client.util.HttpUtil;
import org.apache.commons.collections4.MapUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.lang.NonNull;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AuthenticationException;
import org.springframework.security.oauth2.core.user.OAuth2User;

import java.util.Collections;
import java.util.List;

public class CustomOAuth2UserService extends DefaultOAuth2UserService implements ApplicationContextAware {

    private ApplicationContext applicationContext;

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
        var userAttributesCustomizerList = getAllOAuth2UserAttributesCustomizers();
        for (OAuth2UserAttributesCustomizer customizer : userAttributesCustomizerList) {
            customizer.customize(userAttributes);
        }
        context.setUserAttributes(userAttributes);

        // 3. 设置令牌信息
        context.setAccessToken(userRequest.getAccessToken());
        OAuth2ContextHolder.setContext(context);

        return oAuth2User;
    }

    @Override
    public void setApplicationContext(@NonNull ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }

    private List<OAuth2UserAttributesCustomizer> getAllOAuth2UserAttributesCustomizers() {
        DefaultListableBeanFactory beanFactory = (DefaultListableBeanFactory) applicationContext.getAutowireCapableBeanFactory();
        var beans = beanFactory.getBeansOfType(OAuth2UserAttributesCustomizer.class);
        if (MapUtils.isNotEmpty(beans)) {
            return beans.values().stream().toList();
        } else {
            return Collections.emptyList();
        }
    }
}
