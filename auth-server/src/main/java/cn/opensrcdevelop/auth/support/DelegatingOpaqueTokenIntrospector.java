package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.annotation.Resource;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.security.oauth2.resource.OAuth2ResourceServerProperties;
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
    private final OAuth2ResourceServerProperties resourceServerProperties;

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
                .withIntrospectionUri(rootUrl + INTROSPECT_URI)
                .clientId(resourceServerProperties.getOpaquetoken().getClientId())
                .clientSecret(resourceServerProperties.getOpaquetoken().getClientSecret()).build();
        return delegatedIntrospector.introspect(token);
    }
}
