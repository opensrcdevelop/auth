package cn.opensrcdevelop.auth.support;

import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.common.constants.CommonConstants;
import lombok.RequiredArgsConstructor;
import org.springframework.core.convert.converter.Converter;
import org.springframework.security.oauth2.core.oidc.OidcUserInfo;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.security.oauth2.server.authorization.oidc.authentication.OidcUserInfoAuthenticationContext;
import org.springframework.security.oauth2.server.authorization.oidc.authentication.OidcUserInfoAuthenticationToken;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;

import java.util.List;

@RequiredArgsConstructor
public class OidcUserInfoService implements Converter<OidcUserInfoAuthenticationContext, OidcUserInfo> {

    private final UserService userService;

    @Override
    public OidcUserInfo convert(OidcUserInfoAuthenticationContext context) {
        OidcUserInfoAuthenticationToken authentication =  context.getAuthentication();
        Jwt jwt = ((JwtAuthenticationToken) authentication.getPrincipal()).getToken();
        String userId = jwt.getClaimAsString(JwtClaimNames.SUB);
        List<String> aud = jwt.getClaim(JwtClaimNames.AUD);
        User user = userService.getUserInfo(userId, aud.get(0));
        return OidcUserInfo.builder()
                .subject(user.getUsername())
                .phoneNumber(user.getPhoneNumber())
                .email(user.getEmailAddress())
                .claim(CommonConstants.ROLES, user.getRoles().stream().map(Role::getRoleCode).toList())
                .build();
    }
}
