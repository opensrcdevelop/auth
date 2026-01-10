package cn.opensrcdevelop.auth.configurer;

import cn.opensrcdevelop.auth.biz.component.oauth2login.CustomAuthorizationCodeTokenResponseClient;
import cn.opensrcdevelop.auth.biz.component.oauth2login.CustomOAuth2AuthorizationRequestResolver;
import cn.opensrcdevelop.auth.biz.component.oauth2login.CustomOAuth2LoginAuthenticationProvider;
import cn.opensrcdevelop.auth.biz.component.oauth2login.CustomOAuth2UserService;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.handler.OAuth2LoginFailureHandler;
import cn.opensrcdevelop.auth.handler.OAuth2LoginSuccessHandler;
import cn.opensrcdevelop.common.config.AuthorizationServerProperties;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.ProviderManager;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.oauth2.client.authentication.OAuth2LoginAuthenticationProvider;
import org.springframework.security.oauth2.client.oidc.authentication.OidcAuthorizationCodeAuthenticationProvider;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;

/**
 * OAuth2 Login 配置
 *
 */
@RequiredArgsConstructor
public class OAuth2LoginConfigurer extends AbstractHttpConfigurer<OAuth2LoginConfigurer, HttpSecurity> {

    private final ClientRegistrationRepository clientRegistrationRepository;
    private final CustomOAuth2UserService customOAuth2UserService;
    private final IdentitySourceRegistrationService identitySourceRegistrationService;
    private final AuthorizationServerProperties authorizationServerProperties;

    @Override
    public void init(HttpSecurity http) throws Exception {
        http
                .oauth2Login(x -> {
                    x.successHandler(new OAuth2LoginSuccessHandler());
                    x.failureHandler(new OAuth2LoginFailureHandler());
                    x.clientRegistrationRepository(clientRegistrationRepository);
                    x.authorizationEndpoint(authorization -> authorization
                            .authorizationRequestResolver(new CustomOAuth2AuthorizationRequestResolver(
                                    authorizationServerProperties,
                                    clientRegistrationRepository, identitySourceRegistrationService)));
                    x.redirectionEndpoint(redirection -> redirection
                            .baseUri(authorizationServerProperties.getApiPrefix()
                                    .concat(AuthConstants.FEDERATION_LOGIN_REDIRECTION_URI)));
                    x.userInfoEndpoint(userInfo -> userInfo
                            .userService(customOAuth2UserService));
                });
    }

    @Override
    public void configure(HttpSecurity http) throws Exception {
        // 禁用 OidcAuthorizationCodeAuthenticationProvider 和
        // OAuth2LoginAuthenticationProvider
        // 避免 OIDC id_token 认证失败
        AuthenticationManager authenticationManager = http.getSharedObject(AuthenticationManager.class);
        if (authenticationManager instanceof ProviderManager providerManager) {
            List<AuthenticationProvider> providers = providerManager.getProviders();
            providers.removeIf(OidcAuthorizationCodeAuthenticationProvider.class::isInstance);
            providers.removeIf(OAuth2LoginAuthenticationProvider.class::isInstance);
        }

        http.authenticationProvider(new CustomOAuth2LoginAuthenticationProvider(
                new CustomAuthorizationCodeTokenResponseClient(identitySourceRegistrationService),
                customOAuth2UserService));
    }
}
