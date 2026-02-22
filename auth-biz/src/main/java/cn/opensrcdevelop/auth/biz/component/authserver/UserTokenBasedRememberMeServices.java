package cn.opensrcdevelop.auth.biz.component.authserver;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.user.UserService;
import cn.opensrcdevelop.common.config.AuthorizationServerProperties;
import cn.opensrcdevelop.common.util.CommonUtil;
import cn.opensrcdevelop.common.util.JwtUtil;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.time.temporal.ChronoUnit;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.authentication.RememberMeAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.jwt.JwtClaimNames;
import org.springframework.security.web.authentication.RememberMeServices;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@RequiredArgsConstructor
public class UserTokenBasedRememberMeServices implements RememberMeServices {

    private final UserService userService;
    private final AuthorizationServerProperties authorizationServerProperties;

    @Setter
    @Getter
    private String cookieName = AuthConstants.REMEMBER_ME;

    @Setter
    @Getter
    private String rememberMeParameter = AuthConstants.REMEMBER_ME;

    @Override
    public Authentication autoLogin(HttpServletRequest request, HttpServletResponse response) {
        String rememberMeToken = extractRememberMeCookie(request);
        if (StringUtils.isEmpty(rememberMeToken)) {
            cancelCookie(request, response);
            return null;
        }

        String userId = JwtUtil.getJwtClaimsWithHS256(rememberMeToken).get(JwtClaimNames.SUB).toString();
        if (StringUtils.isEmpty(userId)) {
            cancelCookie(request, response);
            return null;
        }
        String rememberMeTokenSecret = getRememberMeTokenSecret(userId);
        if (StringUtils.isEmpty(rememberMeTokenSecret)
                || !JwtUtil.verifyJwtWithHS256(rememberMeToken, rememberMeTokenSecret)) {
            cancelCookie(request, response);
            return null;
        }

        User user = userService.getById(userId);
        if (Objects.isNull(user) || !user.isEnabled()) {
            cancelCookie(request, response);
            return null;
        }

        RememberMeAuthenticationToken auth = new RememberMeAuthenticationToken(AuthConstants.REMEMBER_ME, user,
                Collections.emptyList());
        auth.setAuthenticated(true);
        return auth;
    }

    @Override
    public void loginFail(HttpServletRequest request, HttpServletResponse response) {
        cancelCookie(request, response);
    }

    @Override
    public void loginSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) {
        if (!rememberMeRequested(request, rememberMeParameter)) {
            return;
        }
        String userId = retrieveUserId(authentication);
        if (StringUtils.isEmpty(userId)) {
            log.warn("UserId is empty, cannot generate remember-me token");
            return;
        }
        setRememberMeTokenToCookie(request, response, userId);
    }

    public void setRememberMeTokenToCookie(HttpServletRequest request, HttpServletResponse response, String userId) {
        String rememberMeTokenSecret = getRememberMeTokenSecret(userId);
        if (StringUtils.isEmpty(rememberMeTokenSecret)) {
            rememberMeTokenSecret = CommonUtil.generateRandomString(32);
            userService.update(Wrappers.<User>lambdaUpdate()
                    .set(User::getRememberMeTokenSecret, rememberMeTokenSecret)
                    .eq(User::getUserId, userId));
        }

        String rememberMeToken = JwtUtil.createJwtWithHS256(
                Map.of(
                        JwtClaimNames.SUB,
                        userId),
                rememberMeTokenSecret,
                authorizationServerProperties.getRememberMeSeconds(),
                ChronoUnit.SECONDS);
        setCookie(rememberMeToken, authorizationServerProperties.getRememberMeSeconds(), request, response);
    }

    public boolean rememberMeRequested(HttpServletRequest request, String parameter) {
        String paramValue = request.getParameter(parameter);
        return paramValue != null && (paramValue.equalsIgnoreCase("true") || paramValue.equalsIgnoreCase("on")
                || paramValue.equalsIgnoreCase("yes") || paramValue.equals("1"));
    }

    protected String extractRememberMeCookie(HttpServletRequest request) {
        Cookie[] cookies = request.getCookies();
        if (cookies == null) {
            return null;
        }
        for (Cookie cookie : cookies) {
            if (this.cookieName.equals(cookie.getName())) {
                return cookie.getValue();
            }
        }
        return null;
    }

    private String retrieveUserId(Authentication authentication) {
        if (authentication.getPrincipal() instanceof User user) {
            return user.getUserId();
        }
        return null;
    }

    private void setCookie(String token, int maxAge, HttpServletRequest request, HttpServletResponse response) {
        Cookie cookie = new Cookie(this.cookieName, token);
        cookie.setMaxAge(maxAge);
        cookie.setPath(getCookiePath(request));
        cookie.setSecure(request.isSecure());
        cookie.setHttpOnly(true);
        response.addCookie(cookie);
    }

    private void cancelCookie(HttpServletRequest request, HttpServletResponse response) {
        Cookie cookie = new Cookie(this.cookieName, null);
        cookie.setMaxAge(0);
        cookie.setPath(getCookiePath(request));
        cookie.setSecure(request.isSecure());
        response.addCookie(cookie);
    }

    private String getCookiePath(HttpServletRequest request) {
        String contextPath = request.getContextPath();
        return (!contextPath.isEmpty()) ? contextPath : "/";
    }

    private String getRememberMeTokenSecret(String userId) {
        return userService
                .getOne(Wrappers.<User>lambdaQuery()
                        .select(User::getUserId, User::getRememberMeTokenSecret)
                        .eq(User::getUserId, userId))
                .getRememberMeTokenSecret();
    }
}
