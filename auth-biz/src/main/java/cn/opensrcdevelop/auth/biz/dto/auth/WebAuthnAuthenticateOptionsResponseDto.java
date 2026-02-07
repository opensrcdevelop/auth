package cn.opensrcdevelop.auth.biz.dto.auth;

import com.fasterxml.jackson.annotation.JsonInclude;
import java.util.List;
import lombok.Data;

/**
 * WebAuthn 认证选项响应 DTO
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WebAuthnAuthenticateOptionsResponseDto {

    /**
     * Base64URL 编码的 Challenge
     */
    private String challenge;

    /**
     * RP ID（域名）
     */
    private String rpId;

    /**
     * 超时时间（毫秒）
     */
    private Long timeout;

    /**
     * 允许的凭证列表
     */
    private List<AllowCredential> allowCredentials;

    /**
     * 用户验证偏好
     */
    private String userVerification;

    @Data
    public static class AllowCredential {
        /**
         * 凭证 ID（Base64URL 编码）
         */
        private String id;

        /**
         * 凭证类型
         */
        private String type;

        /**
         * 传输类型列表
         */
        private String[] transports;
    }
}
