package cn.opensrcdevelop.auth.client.support;

import lombok.Data;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.OAuth2RefreshToken;

import java.io.Serial;
import java.io.Serializable;

@Data
public class OAuth2Context implements Serializable {

    @Serial
    private static final long serialVersionUID = 3540300995697551664L;

    /** 访问令牌 */
    private OAuth2AccessToken accessToken;

    /** 刷新令牌 */
    private OAuth2RefreshToken refreshToken;

    /** 属性 */
    private OAuth2Attributes oAuth2Attributes;
}
