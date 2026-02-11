package cn.opensrcdevelop.auth.configurer;

import cn.opensrcdevelop.auth.authentication.password.ResourceOwnerPasswordAuthenticationConverter;
import cn.opensrcdevelop.auth.authentication.password.ResourceOwnerPasswordAuthenticationProvider;
import cn.opensrcdevelop.auth.biz.component.authserver.OidcUserInfoService;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.filter.CaptchaVerificationCheckFilter;
import cn.opensrcdevelop.auth.filter.ChangePwdCheckFilter;
import cn.opensrcdevelop.auth.filter.MfaValidFilter;
import cn.opensrcdevelop.auth.filter.OAuth2AuthorizeRememberMeAuthenticationFilter;
import cn.opensrcdevelop.auth.handler.LoginFailureHandler;
import cn.opensrcdevelop.auth.handler.LoginSuccessHandler;
import cn.opensrcdevelop.auth.handler.LoginTargetAuthenticationEntryPoint;
import cn.opensrcdevelop.common.config.AuthorizationServerProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.RememberMeAuthenticationProvider;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.oauth2.server.authorization.OAuth2AuthorizationService;
import org.springframework.security.oauth2.server.authorization.config.annotation.web.configurers.OAuth2AuthorizationServerConfigurer;
import org.springframework.security.oauth2.server.authorization.token.OAuth2TokenGenerator;
import org.springframework.security.web.authentication.AuthenticationConverter;
import org.springframework.security.web.authentication.DelegatingAuthenticationConverter;
import org.springframework.security.web.authentication.RememberMeServices;
import org.springframework.security.web.context.SecurityContextHolderFilter;
import org.springframework.security.web.util.UrlUtils;
import org.springframework.security.web.util.matcher.MediaTypeRequestMatcher;
import org.springframework.security.web.util.matcher.RequestMatcher;
import org.springframework.web.filter.CorsFilter;

import java.util.List;

/**
 * 授权服务器配置
 */
@RequiredArgsConstructor
public class AuthorizationServerConfigurer extends AbstractHttpConfigurer<AuthorizationServerConfigurer, HttpSecurity> {

    private final CorsFilter corsFilter;
    private final MfaValidFilter mfaValidFilter;
    private final ChangePwdCheckFilter changePwdCheckFilter;
    private final CaptchaVerificationCheckFilter captchaVerificationCheckFilter;
    private final AuthorizationServerProperties authorizationServerProperties;
    private final OidcUserInfoService oidcUserInfoService;
    private final OAuth2AuthorizationServerConfigurer oauth2AuthorizationServerConfigurer = new OAuth2AuthorizationServerConfigurer();
    private final RememberMeServices rememberMeServices;

    @Override
    public void init(HttpSecurity http) throws Exception {
        // 拦截授权服务器相关端点
        RequestMatcher endpointsMatcher = oauth2AuthorizationServerConfigurer
                .getEndpointsMatcher();
        http
                .securityMatcher(endpointsMatcher)
                .authorizeHttpRequests(x -> x.anyRequest().authenticated());

        // 登录表单处理
        http.formLogin(x -> {
            x.loginProcessingUrl(authorizationServerProperties.getApiPrefix().concat(AuthConstants.LOGIN_URL));
            if (UrlUtils.isAbsoluteUrl(authorizationServerProperties.getLoginPageUrl())) {
                x.successHandler(new LoginSuccessHandler());
                x.failureHandler(new LoginFailureHandler());
            }
        });

        // 登出处理
        http.logout(logout -> logout
                .logoutUrl(authorizationServerProperties.getApiPrefix().concat(AuthConstants.LOGOUT_URL)));

        // 登录页面重定向
        http.exceptionHandling(exception -> exception
                .defaultAuthenticationEntryPointFor(
                        new LoginTargetAuthenticationEntryPoint(authorizationServerProperties.getLoginPageUrl()),
                        new MediaTypeRequestMatcher(MediaType.TEXT_HTML)));

        // 禁用 csrf 和 cors
        http.csrf(AbstractHttpConfigurer::disable);
        http.cors(AbstractHttpConfigurer::disable);
    }

    @Override
    public void configure(HttpSecurity http) throws Exception {
        AuthenticationManager authenticationManager = http.getSharedObject(AuthenticationManager.class);

        // 添加跨域过滤器
        http.addFilter(corsFilter);
        // 添加 MFA 校验过滤器（统一处理 TOTP 和 WebAuthn）
        http.addFilterBefore(mfaValidFilter, SecurityContextHolderFilter.class);
        // 添加变更密码检查过滤器
        http.addFilterBefore(changePwdCheckFilter, SecurityContextHolderFilter.class);
        // 添加图像验证码二次校验过滤器
        http.addFilterBefore(captchaVerificationCheckFilter, ChangePwdCheckFilter.class);
        // 添加 OAuth2 Authorize 记住我过滤器
        http.addFilterAfter(
                new OAuth2AuthorizeRememberMeAuthenticationFilter(authenticationManager, rememberMeServices),
                SecurityContextHolderFilter.class);

        // 自定义授权类型认证提供
        OAuth2TokenGenerator<?> tokenGenerator = http.getSharedObject(OAuth2TokenGenerator.class);
        OAuth2AuthorizationService authorizationService = http.getSharedObject(OAuth2AuthorizationService.class);
        http.authenticationProvider(new ResourceOwnerPasswordAuthenticationProvider(authorizationService,
                authenticationManager, tokenGenerator));
        http.authenticationProvider(new RememberMeAuthenticationProvider(AuthConstants.REMEMBER_ME));
    }

    public OAuth2AuthorizationServerConfigurer getCustomAuthorizationServerConfigurer() {
        // 授权服务器自定义配置
        oauth2AuthorizationServerConfigurer
                .authorizationEndpoint(x -> x.consentPage(authorizationServerProperties.getConsentPageUrl()))
                .tokenEndpoint(x -> x.accessTokenRequestConverter(getCustomAccessTokenRequestConverter()))
                .authorizationServerMetadataEndpoint(x -> x
                        .authorizationServerMetadataCustomizer(c -> c.grantType(AuthConstants.GRANT_TYPE_PASSWORD)))
                .oidc(x -> x
                        .providerConfigurationEndpoint(c -> c
                                .providerConfigurationCustomizer(p -> p.grantType(AuthConstants.GRANT_TYPE_PASSWORD)))
                        .userInfoEndpoint(userinfo -> userinfo.userInfoMapper(oidcUserInfoService::convert)));
        return oauth2AuthorizationServerConfigurer;
    }

    /**
     * 自定义授权类型认证转换器
     */
    private AuthenticationConverter getCustomAccessTokenRequestConverter() {
        return new DelegatingAuthenticationConverter(List.of(
                new ResourceOwnerPasswordAuthenticationConverter()));
    }
}
