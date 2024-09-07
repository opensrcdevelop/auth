package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.common.util.WebUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.autoconfigure.security.oauth2.resource.OAuth2ResourceServerProperties;
import org.springframework.security.oauth2.core.OAuth2AuthenticatedPrincipal;
import org.springframework.security.oauth2.server.resource.introspection.OpaqueTokenIntrospector;
import org.springframework.security.oauth2.server.resource.introspection.SpringOpaqueTokenIntrospector;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DelegatingOpaqueTokenIntrospector implements OpaqueTokenIntrospector {

    private static final String INTROSPECT_URI = "/oauth2/introspect";
    private final OAuth2ResourceServerProperties resourceServerProperties;

    @Override
    public OAuth2AuthenticatedPrincipal introspect(String token) {
        String rootUrl = WebUtil.getRootUrl();
        SpringOpaqueTokenIntrospector delegatedIntrospector = new SpringOpaqueTokenIntrospector(rootUrl + INTROSPECT_URI,
                resourceServerProperties.getOpaquetoken().getClientId(), resourceServerProperties.getOpaquetoken().getClientSecret());
        return delegatedIntrospector.introspect(token);
    }
}
