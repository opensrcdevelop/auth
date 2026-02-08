package cn.opensrcdevelop.auth.biz.dto.auth;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

/**
 * WebAuthn 注册完成请求 DTO
 */
@Data
public class WebAuthnRegisterCompleteRequestDto {

    /**
     * 凭证 ID（Base64URL 编码）
     */
    @NotBlank
    private String id;

    /**
     * 原始凭证 ID（Base64URL 编码）
     */
    @NotBlank
    private String rawId;

    /**
     * 响应对象
     */
    @NotNull
    private Response response;

    /**
     * 传输类型
     */
    @NotNull
    private String transports;

    /**
     * 响应内部类
     */
    @Data
    public static class Response {

        /**
         * 客户端数据JSON（Base64URL 编码）
         */
        @NotBlank
        private String clientDataJSON;

        /**
         * 证明对象（Base64URL 编码，包含公钥信息）
         */
        @NotBlank
        private String attestationObject;
    }
}
