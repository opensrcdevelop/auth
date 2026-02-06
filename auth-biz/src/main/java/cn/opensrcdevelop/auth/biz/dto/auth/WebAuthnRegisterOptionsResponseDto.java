package cn.opensrcdevelop.auth.biz.dto.auth;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

/**
 * WebAuthn 注册选项响应 DTO
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class WebAuthnRegisterOptionsResponseDto {

    /**
     * Base64URL 编码的 Challenge
     */
    private String challenge;

    /**
     * 用户信息
     */
    private User user;

    /**
     * RP 配置信息
     */
    private Rp rp;

    /**
     * 凭证参数列表
     */
    private PubKeyCredParams[] pubKeyCredParams;

    /**
     * 超时时间（毫秒）
     */
    private Long timeout;

    /**
     * 认证器选择条件
     */
    private AuthenticatorSelection authenticatorSelection;

    /**
     * 是否需要用户验证
     */
    private Boolean requireResidentKey;

    /**
     * 排除的凭证 ID 列表
     */
    private String[] excludeCredentials;

    @Data
    public static class User {

        /**
         * 用户 ID（字节数组 Base64URL 编码）
         */
        private String id;

        /**
         * 用户名
         */
        private String name;

        /**
         * 显示名称
         */
        private String displayName;
    }

    @Data
    public static class Rp {

        /**
         * RP ID（域名）
         */
        private String id;

        /**
         * RP 名称
         */
        private String name;
    }

    @Data
    public static class PubKeyCredParams {

        /**
         * 凭证类型
         */
        private String type;

        /**
         * 算法类型（COSE）
         */
        private Integer alg;
    }

    @Data
    public static class AuthenticatorSelection {

        /**
         * 认证器附件模式
         */
        private String authenticatorAttachment;

        /**
         * 是否需要用户验证
         */
        private Boolean userVerification;

        /**
         * 设备类型偏好
         */
        private String residentKey;
    }
}
