package cn.opensrcdevelop.auth.biz.entity.auth;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.time.Instant;

/**
 * 授权信息实体
 */
@Data
@TableName("t_authorization")
public class Authorization implements Serializable {

    @Serial
    private static final long serialVersionUID = -4765719855341393851L;

    @TableId(type = IdType.AUTO)
    private Long id;

    private String registeredClientId;

    private String loginId;

    private String principalName;
    private String authorizationGrantType;
    private String authorizedScopes;
    private String attributes;
    private String state;

    private String authorizationCodeValue;
    private Instant authorizationCodeIssuedAt;
    private Instant authorizationCodeExpiresAt;
    private String authorizationCodeMetadata;

    private String accessTokenValue;
    private Instant accessTokenIssuedAt;
    private Instant accessTokenExpiresAt;
    private String accessTokenMetadata;
    private String accessTokenType;
    private String accessTokenScopes;

    private String refreshTokenValue;
    private Instant refreshTokenIssuedAt;
    private Instant refreshTokenExpiresAt;
    private String refreshTokenMetadata;

    private String oidcIdTokenValue;
    private Instant oidcIdTokenIssuedAt;
    private Instant oidcIdTokenExpiresAt;
    private String oidcIdTokenMetadata;
    private String oidcIdTokenClaims;

    private String userCodeValue;
    private Instant userCodeIssuedAt;
    private Instant userCodeExpiresAt;
    private String userCodeMetadata;

    private String deviceCodeValue;
    private Instant deviceCodeIssuedAt;
    private Instant deviceCodeExpiresAt;
    private String deviceCodeMetadata;
}
