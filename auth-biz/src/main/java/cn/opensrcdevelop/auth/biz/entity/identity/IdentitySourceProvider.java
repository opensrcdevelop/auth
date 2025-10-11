package cn.opensrcdevelop.auth.biz.entity.identity;

import cn.opensrcdevelop.auth.audit.annotation.EntityName;
import cn.opensrcdevelop.auth.audit.annotation.PropertyName;
import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serial;
import java.io.Serializable;

/**
 * 身份源提供商实体
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("t_identity_source_provider")
@EntityName("身份源提供商")
public class IdentitySourceProvider extends BaseEntity implements Serializable {

    @Serial
    private static final long serialVersionUID = 2043941502025970769L;

    @TableId(type = IdType.INPUT)
    @PropertyName("身份源提供商ID")
    private String providerId;

    /** 身份源提供商名称 */
    @PropertyName("身份源提供商名称")
    private String providerName;

    /** 身份源提供商标识 */
    @PropertyName("身份源提供商标识")
    private String providerCode;

    /** 身份源提供商描述 */
    @PropertyName("身份源提供商描述")
    private String providerDesc;

    /** 身份源提供商 Logo */
    @PropertyName("身份源提供商 Logo")
    private String providerLogo;

    /** 授权地址 */
    @PropertyName("授权地址")
    private String authorizationUri;

    /** 令牌地址 */
    @PropertyName("令牌地址")
    private String tokenUri;

    /** 用户信息地址 */
    @PropertyName("用户信息地址")
    private String userInfoUris;

    /** 用户信息认证方式 */
    @PropertyName("用户信息认证方式")
    private String userInfoAuthenticationMethod;

    /** scopes */
    @PropertyName("scopes")
    private String scopes;

    /** 用户名属性 */
    @PropertyName("用户名属性")
    private String usernameAttribute;

    /** 用户匹配属性 */
    @PropertyName("用户匹配属性")
    private String userMatchAttribute;

    /** 唯一标识ID属性 */
    @PropertyName("唯一标识ID属性")
    private String uniqueIdAttribute;

    /** JWK 地址 */
    @PropertyName("JWK 地址")
    private String jwkSetUri;

    /** ISSUER 地址 */
    @PropertyName("ISSUER 地址")
    private String issuerUri;

    /** 源数据 */
    @PropertyName("源数据")
    private String metaData;

    /** 是否启用自定义授权请求 */
    @PropertyName("是否启用自定义授权请求")
    private Boolean enableCustomAuthzReq;

    /** 授权请求配置 */
    @PropertyName("授权请求配置")
    private String authzReqCfg;

    /** 是否启用自定义令牌请求 */
    @PropertyName("是否启用自定义令牌请求")
    private Boolean enableCustomTokenReq;

    /** 令牌请求配置 */
    @PropertyName("令牌请求配置")
    private String tokenReqCfg;

    /** 是否启用自定义用户信息请求 */
    @PropertyName("是否启用自定义用户信息请求")
    private Boolean enableCustomUserInfoReq;

    /** 用户信息请求配置 */
    @PropertyName("用户信息请求配置")
    private String userInfoReqCfg;
}
