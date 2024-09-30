package cn.opensrcdevelop.auth.authentication.email;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import org.springframework.security.authentication.AbstractAuthenticationToken;

import java.io.Serial;
import java.util.Collections;

/**
 * 邮箱验证码登录认证 Token
 */
@Getter
@EqualsAndHashCode(callSuper = true)
public class EmailCodeAuthenticationToken extends AbstractAuthenticationToken {

    @Serial
    private static final long serialVersionUID = 1019753387502557441L;

    /** 邮箱地址 */
    private final String email;

    /** 请求验证码 */
    private final String requestCode;

    public EmailCodeAuthenticationToken(String email, String requestCode) {
        super(Collections.emptyList());
        this.email = email;
        this.requestCode = requestCode;
        super.setAuthenticated(false);
    }

    @Override
    public Object getCredentials() {
        return "";
    }

    @Override
    public Object getPrincipal() {
        return email;
    }
}
