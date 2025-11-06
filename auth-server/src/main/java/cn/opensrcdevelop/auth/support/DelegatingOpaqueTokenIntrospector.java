package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.auth.biz.constants.SystemSettingConstants;
import cn.opensrcdevelop.auth.biz.service.system.SystemSettingService;
import cn.opensrcdevelop.common.config.AuthorizationServerProperties;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.oauth2.core.OAuth2AuthenticatedPrincipal;
import org.springframework.security.oauth2.jwt.JwtDecoder;
import org.springframework.security.oauth2.server.resource.authentication.BearerTokenAuthenticationToken;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationProvider;
import org.springframework.security.oauth2.server.resource.introspection.OpaqueTokenIntrospector;
import org.springframework.security.oauth2.server.resource.introspection.SpringOpaqueTokenIntrospector;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DelegatingOpaqueTokenIntrospector implements OpaqueTokenIntrospector {

    private static final String INTROSPECT_URI = "/oauth2/introspect";
    private final SystemSettingService systemSettingService;
    private final AuthorizationServerProperties authorizationServerProperties;

    @Lazy
    @Resource
    private JwtDecoder jwtDecoder;

    @Override
    public OAuth2AuthenticatedPrincipal introspect(String token) {
        BearerTokenAuthenticationToken bearerTokenAuthenticationToken = new BearerTokenAuthenticationToken(token);
        JwtAuthenticationProvider jwtAuthenticationProvider = new JwtAuthenticationProvider(jwtDecoder);
        jwtAuthenticationProvider.authenticate(bearerTokenAuthenticationToken);

        String rootUrl = WebUtil.getRootUrl();
        SpringOpaqueTokenIntrospector delegatedIntrospector = SpringOpaqueTokenIntrospector
                .withIntrospectionUri(rootUrl + authorizationServerProperties.getApiPrefix() + INTROSPECT_URI)
                .clientId(systemSettingService.getSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_ID, String.class))
                .clientSecret(systemSettingService.getSystemSetting(SystemSettingConstants.CONSOLE_CLIENT_SECRET, String.class)).build();
        return delegatedIntrospector.introspect(token);
    }
}
