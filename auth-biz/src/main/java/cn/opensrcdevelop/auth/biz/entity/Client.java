package cn.opensrcdevelop.auth.biz.entity;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;
import java.time.Instant;

/**
 * 客户端实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_client")
public class Client extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -7100176093378154651L;

    @TableId(type = IdType.INPUT)
    private String clientId;

    private Instant clientIdIssuedAt;

    private String clientSecret;

    private Instant clientSecretExpiresAt;

    private String clientName;

    private String clientAuthenticationMethods;

    private String authorizationGrantTypes;

    private String redirectUris;

    private String postLogoutRedirectUris;

    private String scopes;

    private String clientSettings;

    private String tokenSettings;

    private String description;
}
