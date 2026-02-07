package cn.opensrcdevelop.auth.biz.service.auth;

import cn.opensrcdevelop.auth.biz.dto.auth.*;
import cn.opensrcdevelop.auth.biz.entity.user.User;
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
     *            用户ID（未登录时为 null）
     * @param request
     *            HTTP请求
     * @return 认证选项
     */
    WebAuthnAuthenticateOptionsResponseDto getAuthenticationOptions(String userId, HttpServletRequest request);

    /**
     * 完成认证
     *
     * @param userId
     *            用户ID（未登录时为 null）
     * @param requestDto
     *            认证完成请求
     * @param request
     *            HTTP请求
     * @return 认证成功的用户信息（认证失败返回 null）
     */
    User completeAuthentication(String userId, WebAuthnAuthenticateCompleteRequestDto requestDto,
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
     * 统计用户的凭证数量
     *
     * @param userId
     *            用户ID
     * @return 凭证数量
     */
    long countCredentials(String userId);
}
