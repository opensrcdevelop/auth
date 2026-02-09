package cn.opensrcdevelop.auth.controller;

import cn.opensrcdevelop.auth.biz.dto.auth.*;
import cn.opensrcdevelop.auth.biz.service.auth.WebAuthnService;
import cn.opensrcdevelop.auth.biz.util.AuthUtil;
import cn.opensrcdevelop.common.annoation.RestResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;

import java.util.List;

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
    public void completeRegistration(
            @RequestBody @Valid WebAuthnRegisterCompleteRequestDto requestDto,
            HttpServletRequest request, HttpServletResponse response) {
        webAuthnService.completeRegistration(AuthUtil.getCurrentUserId(), requestDto, request, response);
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
            HttpServletRequest request, HttpServletResponse response) {
        String userId = AuthUtil.getCurrentUserId();
        return webAuthnService.completeAuthentication(userId, requestDto, request, response) != null;
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
