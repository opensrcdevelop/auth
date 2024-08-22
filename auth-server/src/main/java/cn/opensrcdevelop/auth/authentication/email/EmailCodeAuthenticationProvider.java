package cn.opensrcdevelop.auth.authentication.email;

import cn.opensrcdevelop.auth.biz.entity.User;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetailsService;

import java.util.Collections;

/**
 * 邮箱验证码登录认证提供者
 */
@RequiredArgsConstructor
@Slf4j
public class EmailCodeAuthenticationProvider implements AuthenticationProvider {

    private final UserDetailsService userDetailsService;

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        EmailCodeAuthenticationToken authenticationToken = (EmailCodeAuthenticationToken) authentication;
        String email = authenticationToken.getEmail();
        String code = authenticationToken.getCode();
        String requestCode = authenticationToken.getRequestCode();

        if (StringUtils.isEmpty(email) || StringUtils.isEmpty(requestCode)) {
            throw new AuthenticationServiceException("Email or request code not found");
        }

        User user = (User) userDetailsService.loadUserByUsername(email);
        if (user == null) {
            throw new AuthenticationServiceException("Cannot get user info");
        }

        if (!StringUtils.equals(code, requestCode)) {
            throw new AuthenticationServiceException("Invalid request code");
        }
        // 避免 EmailCodeAuthenticationToken 反序列化异常
        return new UsernamePasswordAuthenticationToken(user, "", Collections.emptyList());
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return EmailCodeAuthenticationToken.class.isAssignableFrom(authentication);
    }
}
