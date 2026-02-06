package cn.opensrcdevelop.auth.biz.dto.auth;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

/**
 * WebAuthn 认证完成请求 DTO
 */
@Data
public class WebAuthnAuthenticateCompleteRequestDto {

    /**
     * 凭证 ID（Base64URL 编码）
     */
    @NotBlank(message = "凭证ID不能为空")
    private String id;

    /**
     * 原始凭证响应（Base64URL 编码）
     */
    @NotBlank(message = "原始凭证响应不能为空")
    private String response;

    /**
     * 客户端数据JSON（Base64URL 编码）
     */
    private String clientDataJSON;

    /**
     * 签名（Base64URL 编码）
     */
    private String signature;
}
