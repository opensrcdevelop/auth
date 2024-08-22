package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.auth.client.support.OAuth2UserAttributesCustomizer;
import cn.opensrcdevelop.auth.component.AuthorizationServerProperties;
import cn.opensrcdevelop.auth.configurer.AuthorizationServerConfigurer;
import cn.opensrcdevelop.auth.configurer.ResourceServerConfigurer;
import cn.opensrcdevelop.auth.filter.ChangePwdCheckFilter;
import cn.opensrcdevelop.auth.filter.TotpValidFilter;
import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.util.RedisUtil;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import com.nimbusds.jose.jwk.JWKSet;
import com.nimbusds.jose.jwk.RSAKey;
import com.nimbusds.jose.jwk.source.ImmutableJWKSet;
import com.nimbusds.jose.jwk.source.JWKSource;
import com.nimbusds.jose.proc.SecurityContext;
import io.vavr.control.Try;
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

import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

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
        excludePathPatterns.add(controllerPathPrefix + "/user/changePwd");
        excludePathPatterns.add(controllerPathPrefix + "/user/resetPwd");

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
        excludePathPatterns.add(controllerPathPrefix + "/user/changePwd");
        excludePathPatterns.add(controllerPathPrefix + "/code/email/*");
        excludePathPatterns.add(controllerPathPrefix + "/code/check");
        excludePathPatterns.add(controllerPathPrefix + "/user/resetPwd");

        changePwdCheckFilter.excludePathPatterns(excludePathPatterns.toArray(new String[0]));
        return changePwdCheckFilter;
    }

    /**
     * An instance of com.nimbusds.jose.jwk.source.JWKSource for signing access tokens.
     */
    @Bean
    public JWKSource<SecurityContext> jwkSource() {
        String jwkSourceStr = RedisUtil.get(AuthConstants.JWK_REDIS_KEY, String.class);
        JWKSet jwkSet;
        if (StringUtils.isNotEmpty(jwkSourceStr)) {
            jwkSet = Try.of(() -> JWKSet.parse(jwkSourceStr)).getOrElseThrow(ServerException::new);
        } else {
            KeyPair keyPair = generateRsaKey();
            RSAPublicKey publicKey = (RSAPublicKey) keyPair.getPublic();
            RSAPrivateKey privateKey = (RSAPrivateKey) keyPair.getPrivate();
            RSAKey rsaKey = new RSAKey.Builder(publicKey)
                    .privateKey(privateKey)
                    .keyID(UUID.randomUUID().toString())
                    .build();
            jwkSet = new JWKSet(rsaKey);
            RedisUtil.set(AuthConstants.JWK_REDIS_KEY, jwkSet.toString(false));
        }
        return new ImmutableJWKSet<>(jwkSet);
    }

    /**
     * 	An instance of java.security.KeyPair with keys generated on startup used to create the JWKSource above.
     */
    private static KeyPair generateRsaKey() {
        KeyPair keyPair;
        try {
            KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
            keyPairGenerator.initialize(2048);
            keyPair = keyPairGenerator.generateKeyPair();
        }
        catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
        return keyPair;
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
        return AuthorizationServerSettings.builder().build();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Bean
    public OAuth2UserAttributesCustomizer oAuth2UserAttributesCustomizer() {
        return oAuth2UserAttributes -> {
            // 1. 获取用户信息
            String userId = AuthUtil.getCurrentJwtClaim(JwtClaimNames.SUB);
            List<String> aud = AuthUtil.getCurrentJwtClaim(JwtClaimNames.AUD);
            if (StringUtils.isNotEmpty(userId) && CollectionUtils.isNotEmpty(aud)) {
                UserService userService = SpringContextUtil.getBean(UserService.class);
                User user = userService.getUserInfo(userId, aud.get(0));

                // 2. 获取用户 map
                var userMap = AuthUtil.convertUserMap(user);

                // 3. 设置用户属性
                userMap.forEach(oAuth2UserAttributes::setAttribute);
                oAuth2UserAttributes.setAttribute("ip", WebUtil.getRemoteIP());
                oAuth2UserAttributes.setAttribute("device", WebUtil.getDeviceType());
                oAuth2UserAttributes.setAttribute("browser", WebUtil.getBrowserType());
            }
        };
    }

    @PostConstruct
    public void init() {
        RedisUtil.setRedisTemplate(stringRedisTemplate);
    }
}
