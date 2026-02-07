package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateOptionsResponseDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnCredentialResponseDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterOptionsResponseDto;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.annoation.RestResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.validation.Valid;
import java.util.List;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * WebAuthn 控制器
 */
@Slf4j
@Tag(name = "API-WebAuthn", description = "接口-WebAuthn")
@RestController
@RestResponse
@RequestMapping("/webauthn")
@RequiredArgsConstructor
public class WebAuthnController {

    private final WebAuthnService webAuthnService;

    @Operation(summary = "获取注册选项", description = "获取 WebAuthn 注册所需的选项")
    @PostMapping("/register/options")
    public WebAuthnRegisterOptionsResponseDto getRegistrationOptions(HttpServletRequest request) {
        String userId = AuthUtil.getCurrentUserId();
        return webAuthnService.getRegistrationOptions(userId, request);
    }

    @Operation(summary = "完成注册", description = "完成 WebAuthn 凭证注册")
    @PostMapping("/register/complete")
    public WebAuthnCredentialResponseDto completeRegistration(
            @RequestBody @Valid WebAuthnRegisterCompleteRequestDto requestDto,
            HttpServletRequest request) {
        String userId = AuthUtil.getCurrentUserId();
        log.info("completeRegistration called - userId: {}, credentialId: {}", userId, requestDto.getId());
        return webAuthnService.completeRegistration(userId, requestDto, request);
    }

    @Operation(summary = "获取认证选项", description = "获取 WebAuthn/Passkey 认证所需的选项（支持已登录和未登录场景）")
    @PostMapping("/authenticate/options")
    public WebAuthnAuthenticateOptionsResponseDto getAuthenticationOptions(HttpServletRequest request) {
        String userId = AuthUtil.getCurrentUserId();
        return webAuthnService.getAuthenticationOptions(userId, request);
    }

    @Operation(summary = "完成认证", description = "完成 WebAuthn 凭证认证")
    @PostMapping("/authenticate/complete")
    public boolean completeAuthentication(
            @RequestBody @Valid WebAuthnAuthenticateCompleteRequestDto requestDto,
            HttpServletRequest request) {
        String userId = AuthUtil.getCurrentUserId();
        return webAuthnService.completeAuthentication(userId, requestDto, request) != null;
    }

    @Operation(summary = "列出凭证", description = "列出当前用户的所有 WebAuthn 凭证")
    @GetMapping("/credentials")
    public List<WebAuthnCredentialResponseDto> listCredentials() {
        String userId = AuthUtil.getCurrentUserId();
        return webAuthnService.listCredentials(userId);
    }

    @Operation(summary = "删除凭证", description = "删除指定的 WebAuthn 凭证")
    @DeleteMapping("/credentials/{credentialId}")
    public void deleteCredential(@PathVariable String credentialId) {
        String userId = AuthUtil.getCurrentUserId();
        webAuthnService.deleteCredential(credentialId, userId);
    }
}
