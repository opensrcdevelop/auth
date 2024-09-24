package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.auth.client.support.OAuth2AttributesCustomizer;
import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import cn.opensrcdevelop.auth.configurer.AuthorizationServerConfigurer;
import cn.opensrcdevelop.auth.configurer.ResourceServerConfigurer;
import cn.opensrcdevelop.auth.filter.ChangePwdCheckFilter;
import cn.opensrcdevelop.auth.filter.TotpValidFilter;
import cn.opensrcdevelop.auth.support.DelegatingJWKSource;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.nimbusds.jose.jwk.source.JWKSource;
import com.nimbusds.jose.proc.SecurityContext;
import jakarta.annotation.PostConstruct;
import lombok.RequiredArgsConstructor;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.server.authorization.config.annotation.web.configuration.OAuth2AuthorizationServerConfiguration;
import org.springframework.security.oauth2.server.authorization.settings.AuthorizationServerSettings;
import org.springframework.security.oauth2.server.resource.introspection.OpaqueTokenIntrospector;
import org.springframework.security.web.SecurityFilterChain;
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

    @Value("${spring.controller.path-prefix}")
    private String controllerPathPrefix;

    @Bean
    public SecurityFilterChain authorizationServerSecurityFilterChain(HttpSecurity http, AuthorizationServerProperties authorizationServerProperties) throws Exception {
        http.with(new AuthorizationServerConfigurer(corsFilter(), totpValidFilter(), changePwdCheckFilter(), authorizationServerProperties), x-> {});
        return http.build();
    }

    @Bean
    public SecurityFilterChain resourceServerSecurityFilterChain(HttpSecurity http, AuthorizationServerProperties authorizationServerProperties) throws Exception {
        http.with(new ResourceServerConfigurer(corsFilter(), totpValidFilter(), changePwdCheckFilter(), authorizationServerProperties, tokenIntrospector), x -> {});
        return http.build();
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
        excludePathPatterns.add(controllerPathPrefix + "/docs/**");
        excludePathPatterns.add(controllerPathPrefix + "/totp/check");
        excludePathPatterns.add(controllerPathPrefix + "/code/email/*");
        excludePathPatterns.add(controllerPathPrefix + "/code/check");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/change");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/reset");
        excludePathPatterns.add(controllerPathPrefix + "/tenant/check/*");

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
        excludePathPatterns.add(controllerPathPrefix + "/docs/**");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/change");
        excludePathPatterns.add(controllerPathPrefix + "/code/email/*");
        excludePathPatterns.add(controllerPathPrefix + "/code/check");
        excludePathPatterns.add(controllerPathPrefix + "/user/me/password/reset");
        excludePathPatterns.add(controllerPathPrefix + "/tenant/check/*");

        changePwdCheckFilter.excludePathPatterns(excludePathPatterns.toArray(new String[0]));
        return changePwdCheckFilter;
    }

    /**
     * An instance of com.nimbusds.jose.jwk.source.JWKSource for signing access tokens.
     */
    @Bean
    public JWKSource<SecurityContext> jwkSource() {
        return new DelegatingJWKSource();
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
                User user = userService.getUserInfo(userId, aud.get(0));

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

    @PostConstruct
    public void init() {
        RedisUtil.setRedisTemplate(stringRedisTemplate);
    }
}
