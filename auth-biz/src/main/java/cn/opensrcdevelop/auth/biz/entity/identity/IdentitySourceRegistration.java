package cn.opensrcdevelop.auth.biz.entity.identity;

import cn.opensrcdevelop.auth.biz.constants.AuthConstants;
import cn.opensrcdevelop.common.constants.CommonConstants;
import cn.opensrcdevelop.common.entity.BaseEntity;
import cn.opensrcdevelop.common.util.CommonUtil;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.core.type.TypeReference;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.core.AuthenticationMethod;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.oauth2.core.ClientAuthenticationMethod;
import org.springframework.util.StringUtils;

import java.io.Serial;
import java.io.Serializable;
import java.util.Map;

/**
 * 身份源注册实体
 *
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_identity_source_registration")
public class IdentitySourceRegistration extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 4677096632230142420L;

    @TableId(type = IdType.INPUT)
    private String registrationId;

    /** 身份源提供商 ID */
    private String providerId;

    /** 身份源注册名称 */
    private String registrationName;

    /** 身份源注册标识 */
    private String registrationCode;

    /** 客户端 ID */
    private String clientId;

    /** 客户端密钥 */
    private String clientSecret;

    /** 客户端认证方式 */
    private String clientAuthenticationMethod;

    /** 授权模式 */
    private String authorizationGrantType;

    /** 是否启用 */
    private Boolean enabled;

    /** 附加参数 */
    private String additionalParams;

    @TableField(exist = false)
    private IdentitySourceProvider identitySourceProvider;

    public ClientRegistration toClientRegistration() {
        ClientRegistration.Builder builder = ClientRegistration.withRegistrationId(registrationCode);
        // 身份源提供商信息
        builder.userNameAttributeName(identitySourceProvider.getUsernameAttribute());
        builder.authorizationUri(identitySourceProvider.getAuthorizationUri());
        builder.tokenUri(identitySourceProvider.getTokenUri());
        builder.userInfoUri(StringUtils.delimitedListToStringArray(identitySourceProvider.getUserInfoUris(), CommonConstants.COMMA)[0]);
        builder.issuerUri(identitySourceProvider.getIssuerUri());
        builder.jwkSetUri(identitySourceProvider.getJwkSetUri());
        builder.scope(StringUtils.delimitedListToStringArray(identitySourceProvider.getScopes(), CommonConstants.COMMA));
        builder.userInfoAuthenticationMethod(new AuthenticationMethod(identitySourceProvider.getUserInfoAuthenticationMethod()));
        if (StringUtils.hasText(identitySourceProvider.getMetaData())) {
            builder.providerConfigurationMetadata(CommonUtil.deserializeObject(identitySourceProvider.getMetaData(), new TypeReference<Map<String, Object>>() {}));
        }

        // 客户端信息
        builder.clientId(clientId);
        builder.clientSecret(clientSecret);
        builder.clientAuthenticationMethod(new ClientAuthenticationMethod(clientAuthenticationMethod));
        builder.redirectUri(AuthConstants.FEDERATION_LOGIN_REDIRECT_URI);
        builder.authorizationGrantType(new AuthorizationGrantType(authorizationGrantType));

        return builder.build();
    }
}
