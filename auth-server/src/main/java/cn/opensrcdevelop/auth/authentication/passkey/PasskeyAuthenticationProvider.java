package cn.opensrcdevelop.auth.authentication.passkey;

import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import cn.opensrcdevelop.common.exception.BizException;
import jakarta.servlet.http.HttpServletRequest;
import java.util.Collections;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

/**
 * Passkey 认证提供者
 */
@RequiredArgsConstructor
@Slf4j
public class PasskeyAuthenticationProvider implements AuthenticationProvider {

    private final WebAuthnService webAuthnService;

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        log.info("PasskeyAuthenticationProvider.authenticate 开始");

        PasskeyAuthenticationToken authenticationToken = (PasskeyAuthenticationToken) authentication;
        String credentialId = authenticationToken.getCredentialId();
        String response = authenticationToken.getResponse();
        String clientDataJSON = authenticationToken.getClientDataJSON();
        String signature = authenticationToken.getSignature();

        log.info("Passkey 认证参数: credentialId={}, response={}, clientDataJSON={}, signature={}",
                credentialId, response != null ? response.substring(0, Math.min(50, response.length())) + "..." : null,
                clientDataJSON != null
                        ? clientDataJSON.substring(0, Math.min(50, clientDataJSON.length())) + "..."
                        : null,
                signature != null ? signature.substring(0, Math.min(50, signature.length())) + "..." : null);

        if (credentialId == null || credentialId.isBlank()) {
            throw new AuthenticationServiceException("凭证ID不能为空");
        }

        if (response == null || response.isBlank()) {
            throw new AuthenticationServiceException("认证响应不能为空");
        }

        // 获取 HttpServletRequest
        HttpServletRequest request = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes())
                .getRequest();

        // 构建认证请求 DTO
        WebAuthnAuthenticateCompleteRequestDto requestDto = new WebAuthnAuthenticateCompleteRequestDto();
        requestDto.setId(credentialId);
        requestDto.setResponse(response);
        requestDto.setClientDataJSON(clientDataJSON);
        requestDto.setSignature(signature);

        log.info("Passkey 开始调用 completeAuthentication");

        // 调用认证方法（未登录场景）
        User user;
        try {
            user = webAuthnService.completeAuthentication(null, requestDto, request);
        } catch (BizException e) {
            log.warn("Passkey 认证失败: {}", e.getMessage());
            throw new AuthenticationServiceException(e.getMessage());
        }

        log.info("Passkey 认证成功: userId={}, username={}", user.getUserId(), user.getUsername());

        // 返回认证成功的 Token
        return new UsernamePasswordAuthenticationToken(user, "", Collections.emptyList());
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return PasskeyAuthenticationToken.class.isAssignableFrom(authentication);
    }
}
