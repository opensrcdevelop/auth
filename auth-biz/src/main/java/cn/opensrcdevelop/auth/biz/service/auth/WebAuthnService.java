package cn.opensrcdevelop.auth.biz.service.auth;

import cn.opensrcdevelop.auth.biz.dto.auth.*;
import cn.opensrcdevelop.auth.biz.entity.user.User;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.util.List;

public interface WebAuthnService {

    WebAuthnRegisterOptionsResponseDto getRegistrationOptions(String userId, HttpServletRequest request);

    void completeRegistration(String userId,
            WebAuthnRegisterCompleteRequestDto requestDto, HttpServletRequest request, HttpServletResponse response);

    WebAuthnAuthenticateOptionsResponseDto getAuthenticationOptions(String userId, HttpServletRequest request);

    User completeAuthentication(String userId, WebAuthnAuthenticateCompleteRequestDto requestDto,
            HttpServletRequest request, HttpServletResponse response);

    List<WebAuthnCredentialResponseDto> listCredentials(String userId);

    void deleteCredential(String credentialId, String userId);

    long countCredentials(String userId);

    void clearCredentials(String userId);
}
