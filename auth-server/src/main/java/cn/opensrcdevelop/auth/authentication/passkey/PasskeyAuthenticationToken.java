package cn.opensrcdevelop.auth.authentication.passkey;

import java.io.Serial;
import java.util.Collections;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import org.springframework.security.authentication.AbstractAuthenticationToken;

/**
 * Passkey 登录认证 Token 不需要用户名，credentialId 中已包含用户标识信息
 */
@Getter
@EqualsAndHashCode(callSuper = true)
public class PasskeyAuthenticationToken extends AbstractAuthenticationToken {

    @Serial
    private static final long serialVersionUID = 1019753387502557450L;

    /** 凭证 ID（Base64URL 编码） */
    private final String credentialId;

    /** 认证器数据（Base64URL 编码） */
    private final String response;

    /** 客户端数据 JSON（Base64URL 编码） */
    private final String clientDataJSON;

    /** 签名数据（Base64URL 编码） */
    private final String signature;

    public PasskeyAuthenticationToken(String credentialId, String response, String clientDataJSON, String signature) {
        super(Collections.emptyList());
        this.credentialId = credentialId;
        this.response = response;
        this.clientDataJSON = clientDataJSON;
        this.signature = signature;
        super.setAuthenticated(false);
    }

    @Override
    public Object getCredentials() {
        return response;
    }

    @Override
    public Object getPrincipal() {
        return credentialId;
    }
}
