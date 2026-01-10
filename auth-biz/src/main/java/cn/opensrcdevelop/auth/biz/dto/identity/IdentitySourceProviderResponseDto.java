package cn.opensrcdevelop.auth.biz.dto.identity;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.Builder;
import lombok.Data;

@Schema(description = "身份源提供商响应")
@Data
@Builder
public class IdentitySourceProviderResponseDto {

    @Schema(description = "身份源提供商ID")
    private String id;

    @Schema(description = "身份源提供商名称")
    private String name;

    @Schema(description = "身份源提供商标识")
    private String code;

    @Schema(description = "身份源提供商 logo")
    private String logo;

    @Schema(description = "身份源提供商描述")
    private String desc;

    @Schema(description = "授权地址")
    private String authorizationUri;

    @Schema(description = "令牌地址")
    private String tokenUri;

    @Schema(description = "用户信息地址")
    private List<String> userInfoUris;

    @Schema(description = "用户信息认证方式")
    private String userInfoAuthenticationMethod;

    @Schema(description = "scopes")
    private List<String> scopes;

    @Schema(description = "用户名属性")
    private String usernameAttribute;

    @Schema(description = "唯一标识属性")
    private String uniqueIdAttribute;

    @Schema(description = "用户匹配属性")
    private String userMatchAttribute;

    @Schema(description = "JWK 地址")
    private String jwkSetUri;

    @Schema(description = "是否启用自定义授权请求")
    private Boolean enableCustomAuthzReq;

    @Schema(description = "授权请求配置")
    private String authzReqCfg;

    @Schema(description = "是否启用自定义令牌请求")
    private Boolean enableCustomTokenReq;

    @Schema(description = "令牌请求配置")
    private String tokenReqCfg;

    @Schema(description = "是否启用自定义用户信息请求")
    private Boolean enableCustomUserInfoReq;

    @Schema(description = "用户信息请求配置")
    private String userInfoReqCfg;

}
