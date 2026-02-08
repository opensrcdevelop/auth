package cn.opensrcdevelop.auth.biz.mfa;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

/**
 * MFA 验证会话上下文（统一处理 TOTP 和 WebAuthn）
 */
@Data
public class MfaValidContext implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 是否整体 MFA 验证通过
     */
    private Boolean valid;

    /**
     * 用户ID
     */
    private String userId;

    /**
     * 已验证的 MFA 方式
     */
    private Set<String> validatedMethods = new HashSet<>();

    /**
     * 是否请求记住我
     */
    private boolean rememberMeRequested;

    /**
     * 添加已验证的方法
     *
     * @param method
     *            验证方式（TOTP, WEBAUTHN）
     */
    public void addValidatedMethod(String method) {
        validatedMethods.add(method);
    }

    /**
     * 检查指定方式是否已验证
     *
     * @param method
     *            验证方式
     * @return 是否已验证
     */
    public boolean isMethodValidated(String method) {
        return validatedMethods.contains(method);
    }

    /**
     * 检查整体 MFA 是否通过（任意一种方式验证通过即可） 对于单一 MFA，只需一种方式验证通过
     *
     * @return 是否通过
     */
    public boolean isValid() {
        return Boolean.TRUE.equals(valid) && !validatedMethods.isEmpty();
    }
}
