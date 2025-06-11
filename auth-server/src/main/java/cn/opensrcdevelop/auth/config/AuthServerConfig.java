package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.biz.component.CustomOAuth2UserService;
import cn.opensrcdevelop.auth.biz.component.OidcUserInfoService;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.identity.IdentitySourceRegistrationService;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.auth.client.support.OAuth2AttributesCustomizer;
import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import cn.opensrcdevelop.auth.configurer.AuthorizationServerConfigurer;
import cn.opensrcdevelop.auth.configurer.OAuth2LoginConfigurer;
import cn.opensrcdevelop.auth.configurer.ResourceServerConfigurer;
import cn.opensrcdevelop.auth.filter.CaptchaVerificationCheckFilter;
import cn.opensrcdevelop.auth.filter.ChangePwdCheckFilter;
import cn.opensrcdevelop.auth.filter.TotpValidFilter;
import cn.opensrcdevelop.auth.support.DelegatingJWKSource;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.nimbusds.jose.jwk.source.JWKSource;
import com.nimbusds.jose.proc.SecurityContext;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.server.authorization.config.annotation.web.configuration.OAuth2AuthorizationServerConfiguration;
import org.springframework.security.oauth2.server.authorization.settings.AuthorizationServerSettings;
import org.springframework.security.oauth2.server.resource.introspection.OpaqueTokenIntrospector;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.RememberMeServices;
import org.springframework.security.web.authentication.rememberme.TokenBasedRememberMeServices;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

import java.util.ArrayList;
import java.util.List;

@Configuration
@EnableConfigurationProperties(AuthorizationServerProperties.class)
@RequiredArgsConstructor
public class AuthServerConfig {

    private final StringRedisTemplate stringRedisTemplate;
    private final OpaqueTokenIntrospector tokenIntrospector;
    private final RedissonClient redissonClient;

    @Value("${spring.controller.path-prefix}")
    private String controllerPathPrefix;

    @Bean
    public SecurityFilterChain authorizationServerSecurityFilterChain(HttpSecurity http,
                                                                      AuthorizationServerProperties authorizationServerProperties,
                                                                      RememberMeServices rememberMeServices,
                                                                      OidcUserInfoService oidcUserInfoService,
                                                                      OAuth2LoginConfigurer auth2LoginConfigurer) throws Exception {
        AuthorizationServerConfigurer authorizationServerConfigurer = new AuthorizationServerConfigurer(
                corsFilter(),
                totpValidFilter(),
                changePwdCheckFilter(),
                captchaVerificationCheckFilter(),
                authorizationServerProperties,
                oidcUserInfoService,
                rememberMeServices);
        http.with(authorizationServerConfigurer, x-> {});
        http.with(authorizationServerConfigurer.getCustomAuthorizationServerConfigurer(), x -> {});
        http.with(auth2LoginConfigurer, x -> {});
        return http.build();
    }

    @Bean
    public SecurityFilterChain resourceServerSecurityFilterChain(HttpSecurity http,
                                                                 AuthorizationServerProperties authorizationServerProperties,
                                                                 RememberMeServices rememberMeServices,
                                                                 OAuth2LoginConfigurer auth2LoginConfigurer) throws Exception {
        http.with(new ResourceServerConfigurer(corsFilter(), totpValidFilter(), changePwdCheckFilter(), authorizationServerProperties, tokenIntrospector, rememberMeServices), x -> {});
        http.with(auth2LoginConfigurer, x -> {});
        return http.build();
    }

    @Bean
    public OAuth2LoginConfigurer auth2LoginConfigurer(ClientRegistrationRepository clientRegistrationRepository,
                                                      CustomOAuth2UserService customOAuth2UserService,
                                                      IdentitySourceRegistrationService identitySourceRegistrationService) {
        return new OAuth2LoginConfigurer(clientRegistrationRepository, customOAuth2UserService, identitySourceRegistrationService);
    }

    @Bean
    public CorsFilter corsFilter() {
        CorsConfiguration corsConfiguration = new CorsConfiguration();
        corsConfiguration.addAllowedOriginPattern("*");
        corsConfiguration.setAllowCredentials(true);
        corsConfiguration.addAllowedMethod("*");
        corsConfiguration.addAllowedHeader("*");

        UrlBasedCorsConfigurationSource corsConfigurationSource = new UrlBasedCorsConfigurationSource();
        corsConfigurationSource.registerCorsConfiguration("/**", corsConfiguration);
        return new CorsFilter(corsConfigurationSource);
    }

    @Bean
    public TotpValidFilter totpValidFilter() {
        TotpValidFilter totpValidFilter = new TotpValidFilter();
        controllerPathPrefix = controllerPathPrefix == null ? "" : controllerPathPrefix;
        List<String> excludePathPatterns = new ArrayList<>();
        excludePathPatterns.add("/swagger-ui/**");
        excludePathPatterns.add("/logout");
        excludePathPatterns.add("/login");
        excludePathPatterns.add("/login/email");
        excludePathPatterns.add("/ui/**");
        excludePathPatterns.add(controllerPathPrefix + "/docs/**");
        excludePathPatterns.add(controllerPathPrefix + "/totp/check");
        excludePathPatterns.add(controllerPathPrefix + "/code/email/*");
        excludePathPatterns.add(controllerPathPrefix + "/code/check");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/change");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/reset");
        excludePathPatterns.add(controllerPathPrefix + "/tenant/check/*");
        excludePathPatterns.add(controllerPathPrefix + "/captcha/get");
        excludePathPatterns.add(controllerPathPrefix + "/captcha/check");
        excludePathPatterns.add(controllerPathPrefix + "/setting/passwordPolicy/checkWithoutPolicy");

        totpValidFilter.excludePathPatterns(excludePathPatterns.toArray(new String[0]));
        return totpValidFilter;
    }

    @Bean
    public ChangePwdCheckFilter changePwdCheckFilter() {
        ChangePwdCheckFilter changePwdCheckFilter = new ChangePwdCheckFilter();
        controllerPathPrefix = controllerPathPrefix == null ? "" : controllerPathPrefix;
        List<String> excludePathPatterns = new ArrayList<>();
        excludePathPatterns.add("/logout");
        excludePathPatterns.add("/login");
        excludePathPatterns.add("/login/email");
        excludePathPatterns.add("/swagger-ui/**");
        excludePathPatterns.add("/ui/**");
        excludePathPatterns.add(controllerPathPrefix + "/docs/**");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/change");
        excludePathPatterns.add(controllerPathPrefix + "/code/email/*");
        excludePathPatterns.add(controllerPathPrefix + "/code/check");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/reset");
        excludePathPatterns.add(controllerPathPrefix + "/tenant/check/*");
        excludePathPatterns.add(controllerPathPrefix + "/captcha/get");
        excludePathPatterns.add(controllerPathPrefix + "/captcha/check");
        excludePathPatterns.add(controllerPathPrefix + "/setting/passwordPolicy/checkWithoutPolicy");

        changePwdCheckFilter.excludePathPatterns(excludePathPatterns.toArray(new String[0]));
        return changePwdCheckFilter;
    }

    @Bean
    public CaptchaVerificationCheckFilter captchaVerificationCheckFilter() {
        return new CaptchaVerificationCheckFilter(List.of("/login"));
    }

    /**
     * An instance of com.nimbusds.jose.jwk.source.JWKSource for signing access tokens.
     */
    @Bean
    public JWKSource<SecurityContext> jwkSource(SystemSettingService systemSettingService) {
        return new DelegatingJWKSource(systemSettingService);
    }

    /**
     * An instance of JwtDecoder for decoding signed access tokens.
     */
    @Bean
    public JwtDecoder jwtDecoder(JWKSource<SecurityContext> jwkSource) {
        return OAuth2AuthorizationServerConfiguration.jwtDecoder(jwkSource);
    }

    /**
     * An instance of AuthorizationServerSettings to configure Spring Authorization Server.
     */
    @Bean
    public AuthorizationServerSettings authorizationServerSettings() {
        return AuthorizationServerSettings.builder()
                .multipleIssuersAllowed(true)
                .build();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public OAuth2AttributesCustomizer oAuth2UserAttributesCustomizer() {
        return oAuth2Attributes -> {
            // 1. 获取用户信息
            String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
            List<String> aud = AuthUtil.getCurrentJwtClaim(JwtClaimNames.AUD);
            if (StringUtils.isNotEmpty(userId) && CollectionUtils.isNotEmpty(aud)) {
                UserService userService = SpringContextUtil.getBean(UserService.class);
                User user = userService.getUserInfo(userId, aud.getFirst());

                // 2. 获取用户 map
                var userMap = AuthUtil.convertUserMap(user);

                // 3. 设置属性
                userMap.forEach(oAuth2Attributes::setAttribute);
                oAuth2Attributes.setAttribute("ip", WebUtil.getRemoteIP());
                oAuth2Attributes.setAttribute("device", WebUtil.getDeviceType());
                oAuth2Attributes.setAttribute("browser", WebUtil.getBrowserType());
            }
        };
    }

    @Bean
    public RememberMeServices rememberMeServices(UserDetailsService userDetailsService, AuthorizationServerProperties authorizationServerProperties) {
        String secret = authorizationServerProperties.getRememberMeTokenSecret();
        if (StringUtils.isEmpty(secret)) {
            secret = CommonUtil.getUUIDString();
        }
        TokenBasedRememberMeServices rememberMeServices = new TokenBasedRememberMeServices(secret , userDetailsService);
        rememberMeServices.setTokenValiditySeconds(authorizationServerProperties.getRememberMeSeconds());
        rememberMeServices.setParameter(AuthConstants.REMEMBER_ME);

        return rememberMeServices;
    }

    @PostConstruct
    public void init() {
        RedisUtil.setRedisTemplate(stringRedisTemplate);
        RedisUtil.setRedissonClient(redissonClient);
    }
}
