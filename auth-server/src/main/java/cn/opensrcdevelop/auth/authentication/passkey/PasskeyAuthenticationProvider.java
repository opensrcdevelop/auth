package cn.opensrcdevelop.auth.authentication.passkey;

import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.entity.auth.WebAuthnCredential;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.repository.auth.WebAuthnCredentialRepository;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import java.util.Collections;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationProvider;
import org.springframework.security.authentication.AuthenticationServiceException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetailsService;

/**
 * Passkey 登录认证提供者 根据 credentialId 查找用户并验证 Passkey 认证
 */
@RequiredArgsConstructor
@Slf4j
public class PasskeyAuthenticationProvider implements AuthenticationProvider {

    private final UserDetailsService userDetailsService;
    private final WebAuthnService webAuthnService;
    private final WebAuthnCredentialRepository webAuthnCredentialRepository;

    @Override
    public Authentication authenticate(Authentication authentication) throws AuthenticationException {
        PasskeyAuthenticationToken authenticationToken = (PasskeyAuthenticationToken) authentication;
        String credentialId = authenticationToken.getCredentialId();
        String response = authenticationToken.getResponse();
        String clientDataJSON = authenticationToken.getClientDataJSON();

        if (credentialId == null || credentialId.isBlank()) {
            throw new AuthenticationServiceException("凭证ID不能为空");
        }

        // 1. 根据 credentialId 查找凭证
        WebAuthnCredential credential = webAuthnCredentialRepository.findByCredentialId(credentialId);
        if (credential == null || credential.getDeleted()) {
            throw new AuthenticationServiceException("凭证不存在或已失效");
        }

        // 2. 根据凭证关联的用户ID获取用户信息
        String userId = credential.getUserId();
        User user = (User) userDetailsService.loadUserByUsername(userId);
        if (user == null) {
            throw new AuthenticationServiceException("用户不存在");
        }

        // 3. 构建认证请求 DTO
        WebAuthnAuthenticateCompleteRequestDto requestDto = new WebAuthnAuthenticateCompleteRequestDto();
        requestDto.setId(credentialId);
        requestDto.setResponse(response);
        requestDto.setClientDataJSON(clientDataJSON);

        // 4. 验证 Passkey 认证
        boolean authenticated = webAuthnService.completeAuthentication(
                user.getUserId(), requestDto, null);

        if (!authenticated) {
            throw new AuthenticationServiceException("Passkey 认证失败");
        }

        // 5. 设置 Passkey 登录验证通过状态（避免 LoginSuccessHandler 重复设置 valid=false）
        webAuthnService.setValidForPasskeyLogin(user.getUserId(), null);

        // 6. 返回认证成功的 Token
        return new UsernamePasswordAuthenticationToken(user, "", Collections.emptyList());
    }

    @Override
    public boolean supports(Class<?> authentication) {
        return PasskeyAuthenticationToken.class.isAssignableFrom(authentication);
    }
}
