package cn.opensrcdevelop.auth.client.config;

import cn.opensrcdevelop.auth.client.authorize.AuthorizeExpressionHandler;
import cn.opensrcdevelop.auth.client.service.CustomOAuth2UserService;
import cn.opensrcdevelop.auth.client.service.CustomOidcUserService;
import cn.opensrcdevelop.auth.client.service.PermissionService;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.access.expression.method.MethodSecurityExpressionHandler;
import org.springframework.security.config.annotation.method.configuration.EnableMethodSecurity;
import org.springframework.security.oauth2.client.oidc.userinfo.OidcUserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserService;
import org.springframework.security.oauth2.core.user.OAuth2User;

@EnableConfigurationProperties(AuthClientProperties.class)
@Configuration
@EnableMethodSecurity
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
    public MethodSecurityExpressionHandler methodSecurityExpressionHandler(ApplicationContext applicationContext) {
        AuthorizeExpressionHandler authorizeExpressionHandler = new AuthorizeExpressionHandler();
        authorizeExpressionHandler.setApplicationContext(applicationContext);
        return authorizeExpressionHandler;
    }
}
