package cn.opensrcdevelop.auth.biz.entity.client;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.io.Serial;
import java.io.Serializable;
import java.time.Instant;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 客户端实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_client")
@EntityName("客户端")
public class Client extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = -7100176093378154651L;

    @TableId(type = IdType.INPUT)
    @PropertyName("客户端ID")
    private String clientId;

    @PropertyName("客户端ID颁发时间")
    private Instant clientIdIssuedAt;

    @PropertyName("客户端密钥")
    private String clientSecret;

    @PropertyName("客户端密钥过期时间")
    private Instant clientSecretExpiresAt;

    @PropertyName("客户端名称")
    private String clientName;

    @PropertyName("客户端认证方法")
    private String clientAuthenticationMethods;

    @PropertyName("授权类型")
    private String authorizationGrantTypes;

    @PropertyName("重定向 URI")
    private String redirectUris;

    @PropertyName("注销后重定向 URI")
    private String postLogoutRedirectUris;

    @PropertyName("scopes")
    private String scopes;

    @PropertyName("客户端设置")
    private String clientSettings;

    @PropertyName("令牌设置")
    private String tokenSettings;

    @PropertyName("描述")
    private String description;
}
