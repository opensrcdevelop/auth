package cn.opensrcdevelop.auth.biz.service.auth;

import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnAuthenticateOptionsResponseDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnCredentialResponseDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterCompleteRequestDto;
import cn.opensrcdevelop.auth.biz.dto.auth.WebAuthnRegisterOptionsResponseDto;
import jakarta.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * WebAuthn 服务接口
 */
public interface WebAuthnService {

    /**
     * 获取注册选项
     *
     * @param userId
     *            用户ID
     * @param request
     *            HTTP请求
     * @return 注册选项
     */
    WebAuthnRegisterOptionsResponseDto getRegistrationOptions(String userId, HttpServletRequest request);

    /**
     * 完成注册
     *
     * @param userId
     *            用户ID
     * @param requestDto
     *            注册完成请求
     * @param request
     *            HTTP请求
     * @return 凭证响应
     */
    WebAuthnCredentialResponseDto completeRegistration(String userId,
            WebAuthnRegisterCompleteRequestDto requestDto, HttpServletRequest request);

    /**
     * 获取认证选项
     *
     * @param userId
     *            用户ID
     * @return 认证选项
     */
    WebAuthnAuthenticateOptionsResponseDto getAuthenticationOptions(String userId);

    /**
     * 完成认证
     *
     * @param userId
     *            用户ID
     * @param requestDto
     *            认证完成请求
     * @param request
     *            HTTP请求
     * @return 是否认证成功
     */
    boolean completeAuthentication(String userId, WebAuthnAuthenticateCompleteRequestDto requestDto,
            HttpServletRequest request);

    /**
     * 列出用户的所有凭证
     *
     * @param userId
     *            用户ID
     * @return 凭证列表
     */
    List<WebAuthnCredentialResponseDto> listCredentials(String userId);

    /**
     * 删除凭证
     *
     * @param credentialId
     *            凭证ID
     * @param userId
     *            用户ID
     */
    void deleteCredential(String credentialId, String userId);

    /**
     * 设置 WebAuthn 验证通过状态
     *
     * @param userId
     *            用户ID
     * @param request
     *            HTTP请求
     */
    void setValid(String userId, HttpServletRequest request);

    /**
     * 检查 WebAuthn 是否已验证
     *
     * @param request
     *            HTTP请求
     * @return 是否已验证
     */
    boolean isValidated(HttpServletRequest request);

    /**
     * Passkey 登录认证成功后设置验证通过状态 与 setValid 不同，这个方法在 MFA 场景下直接设置 valid=true， 避免
     * LoginSuccessHandler 重复设置 valid=false
     *
     * @param userId
     *            用户ID
     * @param request
     *            HTTP请求
     */
    void setValidForPasskeyLogin(String userId, HttpServletRequest request);

    /**
     * 获取用户凭证数量
     *
     * @param userId
     *            用户ID
     * @return 凭证数量
     */
    long countCredentials(String userId);
}
