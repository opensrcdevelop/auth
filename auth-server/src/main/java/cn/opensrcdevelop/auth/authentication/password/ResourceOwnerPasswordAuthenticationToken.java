package cn.opensrcdevelop.auth.authentication.password;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import org.springframework.lang.Nullable;
import org.springframework.security.authentication.AbstractAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.oauth2.core.AuthorizationGrantType;

import java.io.Serial;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

/**
 * 密码授权模式 token
 */
@EqualsAndHashCode(callSuper = true)
public class ResourceOwnerPasswordAuthenticationToken extends AbstractAuthenticationToken {

    @Serial
    private static final long serialVersionUID = -6295487915594079504L;

    /** 申请的域 */
    @Getter
    private final Set<String> scopes;

    /** 认证信息 */
    private final Authentication authentication;

    /** 请求附加参数 */
    @Getter
    private final transient Map<String, Object> additionalParameters;

    /** 授权方式 */
    @Getter
    private final AuthorizationGrantType authorizationGrantType;

    public ResourceOwnerPasswordAuthenticationToken(AuthorizationGrantType authorizationGrantType, Authentication clientPrincipal,
                                                    @Nullable
                                                    Set<String> scope,
                                                    @Nullable
                                                    Map<String, Object> additionalParameters) {
        super(Collections.emptyList());
        this.scopes = scope;
        this.authentication = clientPrincipal;
        this.additionalParameters = additionalParameters;
        this.authorizationGrantType = authorizationGrantType;
    }


    @Override
    public Object getCredentials() {
        return "";
    }

    @Override
    public Object getPrincipal() {
        return authentication;
    }
}
