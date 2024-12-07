package cn.opensrcdevelop.auth.handler;

import cn.opensrcdevelop.auth.biz.component.DbOAuth2AuthorizationService;
import cn.opensrcdevelop.common.response.CodeEnum;
import cn.opensrcdevelop.common.response.R;
import cn.opensrcdevelop.common.util.SpringContextUtil;
import cn.opensrcdevelop.common.util.WebUtil;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.server.authorization.OAuth2Authorization;
import org.springframework.security.oauth2.server.authorization.OAuth2TokenType;
import org.springframework.security.oauth2.server.resource.web.BearerTokenResolver;
import org.springframework.security.oauth2.server.resource.web.DefaultBearerTokenResolver;
import org.springframework.security.web.authentication.logout.LogoutHandler;

import java.util.Objects;

public class ClearTokenLogoutHandler implements LogoutHandler {

    private static final DbOAuth2AuthorizationService dbOAuth2AuthorizationService = SpringContextUtil.getBean(DbOAuth2AuthorizationService.class);

    @Override
    public void logout(HttpServletRequest request, HttpServletResponse response, Authentication authentication) {
        // 1. 获取 AccessToken
        BearerTokenResolver bearerTokenResolver = new DefaultBearerTokenResolver();
        String accessToken = bearerTokenResolver.resolve(request);
        if (StringUtils.isNotEmpty(accessToken)) {
            // 2. 校验 AccessToken
            OAuth2Authorization authorization = dbOAuth2AuthorizationService.findByToken(accessToken, OAuth2TokenType.ACCESS_TOKEN);
            if (Objects.isNull(authorization) || Objects.isNull(authorization.getAccessToken()) || !authorization.getAccessToken().isActive()) {
                WebUtil.sendJsonResponse(R.optFail(CodeEnum.RCD40001), HttpStatus.UNAUTHORIZED);
            }

            // 3. 删除该 AccessToken 所属用户下的所有令牌
            if (Objects.nonNull(authorization) && Objects.nonNull(authorization.getPrincipalName())) {
                dbOAuth2AuthorizationService.removeUserTokens(authorization.getPrincipalName());
            }
        }
    }
}
