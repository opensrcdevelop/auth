package cn.opensrcdevelop.auth.authentication.passkey;

import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import cn.opensrcdevelop.common.util.WebUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UsernameNotFoundException;

import java.util.Collections;

/**
 * Passkey 认证提供者
 */
@RequiredArgsConstructor
@Slf4j
public class PasskeyAuthenticationProvider implements AuthenticationProvider {

    private final WebAuthnService webAuthnService;

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        WebAuthnAuthenticateCompleteRequestDto requestDto = getWebAuthnAuthenticateCompleteRequestDto(
                (PasskeyAuthenticationToken) authentication);

        // 调用认证方法
        User user;
        try {
            user = webAuthnService.completeAuthentication(null, requestDto, WebUtil.getRequest().orElse(null), null);
        } catch (Exception e) {
            log.warn("Passkey 认证失败: {}", e.getMessage());
            throw new AuthenticationServiceException(e.getMessage());
        }

        if (user == null) {
            throw new UsernameNotFoundException("user not found");
        }

        if (Boolean.TRUE.equals(user.getLocked())) {
            throw new DisabledException("account is locked");
        }

        log.info("Passkey 认证成功: userId={}, username={}", user.getUserId(), user.getUsername());
        // 返回认证成功的 Token
        return new UsernamePasswordAuthenticationToken(user, "", Collections.emptyList());
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return PasskeyAuthenticationToken.class.isAssignableFrom(authentication);
    }

    private WebAuthnAuthenticateCompleteRequestDto getWebAuthnAuthenticateCompleteRequestDto(
            PasskeyAuthenticationToken authentication) {
        String credentialId = authentication.getCredentialId();
        String response = authentication.getResponse();
        String clientDataJSON = authentication.getClientDataJSON();
        String signature = authentication.getSignature();

        // 构建认证请求 DTO
        WebAuthnAuthenticateCompleteRequestDto requestDto = new WebAuthnAuthenticateCompleteRequestDto();
        requestDto.setId(credentialId);
        requestDto.setResponse(response);
        requestDto.setClientDataJSON(clientDataJSON);
        requestDto.setSignature(signature);
        return requestDto;
    }
}
