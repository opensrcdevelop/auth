package cn.opensrcdevelop.auth.config;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.entity.OidcScope;
import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.service.OidcScopeService;
import cn.opensrcdevelop.auth.biz.service.UserService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.oauth2.core.oidc.StandardClaimNames;
import org.springframework.security.oauth2.core.oidc.endpoint.OidcParameterNames;
import org.springframework.security.oauth2.jwt.JwtClaimsSet;
import org.springframework.security.oauth2.server.authorization.OAuth2TokenType;
import org.springframework.security.oauth2.server.authorization.token.JwtEncodingContext;
import org.springframework.security.oauth2.server.authorization.token.OAuth2TokenCustomizer;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Configuration
@RequiredArgsConstructor
public class JwtTokenCustomizerConfig {

    private final OidcScopeService oidcScopeService;
    private final UserService userService;

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
                    // 1. 获取 scope claim 映射关系
                    List<OidcScope> oidcScopes = oidcScopeService.getScopeClaims(new ArrayList<>(scopes));

                    // 2. 获取用户信息 Map
                    Map<String, Object> userMap = AuthUtil.convertUserMap(userService.getUserInfo(user.getUserId(),  context.getRegisteredClient().getClientId()));

                    // 3. 设置 id_token 的 claim
                    CommonUtil.stream(oidcScopes).forEach(oidcScope ->
                            CommonUtil.stream(oidcScope.getClaims())
                                    .forEach(oidcClaim -> {
                                        Object claimValue = userMap.get(oidcClaim.getUserAttr().getAttrKey());
                                        if (claimValue != null) {
                                            claims.claim(oidcClaim.getClaimName(), claimValue);
                                        }
                                    }));

                    // 4. 需要追加用户角色信息
                    if (scopes.contains(AuthConstants.OIDC_SCOPE_ROLES)) {
                        claims.claim(AuthConstants.OIDC_SCOPE_ROLES, userMap.get(CommonConstants.ROLES));
                    }
                }
            }
        };
    }
}
