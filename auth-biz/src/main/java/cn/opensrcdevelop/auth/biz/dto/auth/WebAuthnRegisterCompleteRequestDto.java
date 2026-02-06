package cn.opensrcdevelop.auth.biz.dto.auth;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

/**
 * WebAuthn 注册完成请求 DTO
 */
@Data
public class WebAuthnRegisterCompleteRequestDto {

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
     * 传输类型
     */
    private String transports;

    /**
     * 证明对象（Base64URL 编码，包含公钥信息）
     */
    private String attestationObject;
}
