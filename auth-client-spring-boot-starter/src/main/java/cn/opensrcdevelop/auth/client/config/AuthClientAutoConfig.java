package cn.opensrcdevelop.auth.client.config;

import cn.opensrcdevelop.auth.client.support.OAuth2UserAttributesCustomizer;
import cn.opensrcdevelop.auth.client.support.PermissionService;
import cn.opensrcdevelop.auth.client.support.CustomOAuth2UserService;
import cn.opensrcdevelop.auth.client.support.CustomOidcUserService;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.user.OAuth2User;

@EnableConfigurationProperties(AuthClientProperties.class)
@Configuration
@EnableMethodSecurity
@Import(SpringContextUtil.class)
public class AuthClientAutoConfig {

    @Bean("pms")
    public PermissionService permissionService(AuthClientProperties authClientProperties) {
        return new PermissionService(authClientProperties);
    }

    @Bean
    public OAuth2UserService<OAuth2UserRequest, OAuth2User> oAuth2UserService() {
        return new CustomOAuth2UserService();
    }

    @Bean
    public OidcUserService oidcUserService() {
        return new CustomOidcUserService();
    }

    @Bean
    @ConditionalOnMissingBean(OAuth2UserAttributesCustomizer.class)
    public OAuth2UserAttributesCustomizer oAuth2UserAttributesCustomizer() {
        return oAuth2UserAttributes -> {
            oAuth2UserAttributes.setAttribute("ip", WebUtil.getRemoteIP());
            oAuth2UserAttributes.setAttribute("device", WebUtil.getDeviceType());
            oAuth2UserAttributes.setAttribute("browser", WebUtil.getBrowserType());
        };
    }
}
