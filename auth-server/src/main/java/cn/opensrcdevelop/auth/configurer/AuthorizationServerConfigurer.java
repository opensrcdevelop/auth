package cn.opensrcdevelop.auth.configurer;

import cn.opensrcdevelop.auth.authentication.password.ResourceOwnerPasswordAuthenticationConverter;
import cn.opensrcdevelop.auth.authentication.password.ResourceOwnerPasswordAuthenticationProvider;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import cn.opensrcdevelop.auth.filter.CaptchaVerificationCheckFilter;
import cn.opensrcdevelop.auth.filter.ChangePwdCheckFilter;
import cn.opensrcdevelop.auth.filter.TotpValidFilter;
import cn.opensrcdevelop.auth.handler.LoginFailureHandler;
import cn.opensrcdevelop.auth.handler.LoginSuccessHandler;
import cn.opensrcdevelop.auth.handler.LoginTargetAuthenticationEntryPoint;
import cn.opensrcdevelop.auth.support.OidcUserInfoService;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.http.MediaType;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.oauth2.server.authorization.OAuth2AuthorizationService;
import org.springframework.security.oauth2.server.authorization.config.annotation.web.configurers.OAuth2AuthorizationServerConfigurer;
import org.springframework.security.oauth2.server.authorization.token.OAuth2TokenGenerator;
import org.springframework.security.oauth2.server.authorization.web.authentication.DelegatingAuthenticationConverter;
import org.springframework.security.web.authentication.AuthenticationConverter;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
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
    private final TotpValidFilter totpValidFilter;
    private final ChangePwdCheckFilter changePwdCheckFilter;
    private final CaptchaVerificationCheckFilter captchaVerificationCheckFilter;
    private final AuthorizationServerProperties authorizationServerProperties;
    private final OidcUserInfoService oidcUserInfoService = new OidcUserInfoService(SpringContextUtil.getBean(UserService.class));

    @Override
    public void init(HttpSecurity http) throws Exception {
        // 拦截授权服务器相关端点
        OAuth2AuthorizationServerConfigurer authorizationServerConfigurer = new OAuth2AuthorizationServerConfigurer();
        RequestMatcher endpointsMatcher = authorizationServerConfigurer
                .getEndpointsMatcher();
        http
                .securityMatcher(endpointsMatcher)
                .authorizeHttpRequests(x -> x.anyRequest().authenticated());

        // 授权服务器自定义配置
        http
                .with(authorizationServerConfigurer, configurer ->  configurer
                        .authorizationEndpoint(x -> x.consentPage(authorizationServerProperties.getConsentPageUrl()))
                        .tokenEndpoint(x -> x.accessTokenRequestConverter(getCustomAccessTokenRequestConverter()))
                        .authorizationServerMetadataEndpoint(x -> x.authorizationServerMetadataCustomizer(c -> c.grantType(AuthConstants.GRANT_TYPE_PASSWORD)))
                        .oidc(x -> x
                                .providerConfigurationEndpoint(c -> c.providerConfigurationCustomizer(p -> p.grantType(AuthConstants.GRANT_TYPE_PASSWORD)))
                                .userInfoEndpoint(userinfo -> userinfo.userInfoMapper(oidcUserInfoService::convert))
                        )
                );

        // 登录表单处理
        http.formLogin(x -> {
            x.loginPage("/login");
            if (UrlUtils.isAbsoluteUrl(authorizationServerProperties.getLoginPageUrl())) {
                x.successHandler(new LoginSuccessHandler());
                x.failureHandler(new LoginFailureHandler());
            }
        });

        // 登录页面重定向
        http.exceptionHandling(exception ->
            exception
                    .defaultAuthenticationEntryPointFor(
                        new LoginTargetAuthenticationEntryPoint(authorizationServerProperties.getLoginPageUrl()),
                        new MediaTypeRequestMatcher(MediaType.TEXT_HTML)
                    )
        );

        // 禁用 csrf 和 cors
        http.csrf(AbstractHttpConfigurer::disable);
        http.cors(AbstractHttpConfigurer::disable);
    }

    @Override
    public void configure(HttpSecurity http) throws Exception {
        // 添加跨域过滤器
        http.addFilter(corsFilter);
        // 添加变更密码检查过滤器
        http.addFilterBefore(changePwdCheckFilter, UsernamePasswordAuthenticationFilter.class);
        // 添加 Totp 校验过滤器
        http.addFilterAfter(totpValidFilter, UsernamePasswordAuthenticationFilter.class);
        // 添加图像验证码二次校验过滤器
        http.addFilterBefore(captchaVerificationCheckFilter, ChangePwdCheckFilter.class);

        // 自定义授权类型认证提供
        OAuth2TokenGenerator<?> tokenGenerator =  http.getSharedObject(OAuth2TokenGenerator.class);
        AuthenticationManager authenticationManager = http.getSharedObject(AuthenticationManager.class);
        OAuth2AuthorizationService authorizationService = http.getSharedObject(OAuth2AuthorizationService.class);
        http.authenticationProvider(new ResourceOwnerPasswordAuthenticationProvider(authorizationService, authenticationManager, tokenGenerator));
    }

    /**
     * 自定义授权类型认证转换器
     */
    private AuthenticationConverter getCustomAccessTokenRequestConverter() {
        return new DelegatingAuthenticationConverter(List.of(
                new ResourceOwnerPasswordAuthenticationConverter()
        ));
    }
}
