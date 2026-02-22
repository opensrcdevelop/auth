package cn.opensrcdevelop.auth.biz.component.authserver;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.entity.client.oidc.OidcScope;
import cn.opensrcdevelop.auth.biz.service.client.oidc.OidcScopeService;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.util.CommonUtil;
import java.util.*;
import lombok.RequiredArgsConstructor;
import org.springframework.core.convert.converter.Converter;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;
import org.springframework.security.oauth2.core.oidc.OidcUserInfo;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.security.oauth2.server.authorization.oidc.authentication.OidcUserInfoAuthenticationContext;
import org.springframework.security.oauth2.server.authorization.oidc.authentication.OidcUserInfoAuthenticationToken;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class OidcUserInfoService implements Converter<OidcUserInfoAuthenticationContext, OidcUserInfo> {

    private final OidcScopeService oidcScopeService;
    private final UserService userService;

    @Override
    public OidcUserInfo convert(OidcUserInfoAuthenticationContext context) {
        OidcUserInfoAuthenticationToken authentication = context.getAuthentication();
        Jwt jwt = ((JwtAuthenticationToken) authentication.getPrincipal()).getToken();
        String userId = jwt.getClaimAsString(JwtClaimNames.SUB);
        List<String> aud = jwt.getClaim(JwtClaimNames.AUD);
        List<String> scopes = jwt.getClaim(OAuth2ParameterNames.SCOPE);
        return OidcUserInfo.builder()
                .claims(claims -> claims.putAll(getClaims(userId, aud.getFirst(), scopes)))
                .build();
    }

    /**
     * 获取 OIDC Claim
     *
     * @param userId
     *            用户ID
     * @param clientId
     *            客户端ID
     * @param scopes
     *            scope
     * @return OIDC Claim
     */
    public Map<String, Object> getClaims(String userId, String clientId, Collection<String> scopes) {
        Map<String, Object> claims = new LinkedHashMap<>();

        // 1. 获取 scope claim 映射关系
        List<OidcScope> oidcScopes = oidcScopeService.getScopeClaims(new ArrayList<>(scopes));

        // 2. 获取用户信息 Map
        Map<String, Object> userMap = AuthUtil.convertUserMap(userService.getUserInfo(userId));

        // 3. 设置 id_token 的 claim
        CommonUtil.stream(oidcScopes).forEach(oidcScope -> CommonUtil.stream(oidcScope.getClaims())
                .forEach(oidcClaim -> {
                    Object claimValue = userMap.get(oidcClaim.getUserAttr().getAttrKey());
                    if (claimValue != null) {
                        claims.put(oidcClaim.getClaimName(), claimValue);
                    }
                }));

        // 4. 需要追加用户角色信息
        if (scopes.contains(AuthConstants.OIDC_SCOPE_ROLES)) {
            claims.put(AuthConstants.OIDC_SCOPE_ROLES, userMap.get(CommonConstants.ROLES));
        }

        return claims;
    }
}
