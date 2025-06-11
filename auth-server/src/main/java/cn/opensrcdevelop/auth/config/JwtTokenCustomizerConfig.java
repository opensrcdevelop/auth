package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.biz.component.OidcUserInfoService;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.common.constants.CommonConstants;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.oauth2.core.oidc.StandardClaimNames;
import org.springframework.security.oauth2.core.oidc.endpoint.OidcParameterNames;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.server.authorization.OAuth2TokenType;
import org.springframework.security.oauth2.server.authorization.token.JwtEncodingContext;
import org.springframework.security.oauth2.server.authorization.token.OAuth2TokenCustomizer;

import java.util.Set;

@Configuration
@RequiredArgsConstructor
public class JwtTokenCustomizerConfig {

    private final OidcUserInfoService oidcUserInfoService;

    /**
     * 自定义 jwt claim
     */
    @Bean
    public OAuth2TokenCustomizer<JwtEncodingContext> tokenCustomizer() {
        return context -> {
            OAuth2TokenType tokenType = context.getTokenType();
            Set<String> scopes = context.getAuthorizedScopes();

            if (context.getPrincipal().getPrincipal() instanceof User user) {
                JwtClaimsSet.Builder claims =  context.getClaims();

                // access_token
                if (OAuth2TokenType.ACCESS_TOKEN.equals(tokenType)) {
                    claims.claim(StandardClaimNames.SUB, user.getUserId());
                    claims.claim(CommonConstants.USERNAME, user.getUsername());
                }

                // id_token
                if (OidcParameterNames.ID_TOKEN.equals(tokenType.getValue())) {
                    oidcUserInfoService.getClaims(user.getUserId(), context.getRegisteredClient().getClientId(), scopes)
                            .forEach(claims::claim);
                }
            }
        };
    }
}
