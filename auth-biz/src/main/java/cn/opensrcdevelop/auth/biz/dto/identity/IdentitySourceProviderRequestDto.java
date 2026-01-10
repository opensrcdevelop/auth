package cn.opensrcdevelop.auth.biz.dto.identity;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;
import java.util.Map;
import lombok.Data;

@Schema(description = "身份源提供商请求")
@Data
public class IdentitySourceProviderRequestDto {

    @Schema(description = "身份源提供商ID")
    @NotBlank(groups = {ValidationGroups.Operation.UPDATE.class})
    private String id;

    @Schema(description = "身份源提供商名称")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String name;

    @Schema(description = "身份源提供商标识")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String code;

    @Schema(description = "身份源提供商 Logo")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String logo;

    @Schema(description = "身份源提供商描述")
    @NotBlankStr
    private String desc;

    @Schema(description = "授权地址")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String authorizationUri;

    @Schema(description = "令牌地址")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String tokenUri;

    @Schema(description = "用户信息地址")
    @NotEmpty(groups = {ValidationGroups.Operation.INSERT.class})
    private List<@NotBlank(groups = {ValidationGroups.Operation.INSERT.class}) @NotBlankStr(groups = {
            ValidationGroups.Operation.UPDATE.class}) String> userInfoUris;

    @Schema(description = "用户信息认证方式")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    @EnumValue({"header", "form", "query"})
    private String userInfoAuthenticationMethod;

    @Schema(description = "scopes")
    private List<String> scopes;

    @Schema(description = "用户名属性")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String usernameAttribute;

    @Schema(description = "唯一标识属性")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String uniqueIdAttribute;

    @Schema(description = "用户匹配属性")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String userMatchAttribute;

    @Schema(description = "JWK 地址")
    private String jwkSetUri;

    @Schema(description = "是否启用自定义授权请求")
    private Boolean enableCustomAuthzReq;

    @Schema(description = "授权请求配置")
    private RequestConfigRequestDto authzReqCfg;

    @Schema(description = "是否启用自定义令牌请求")
    private Boolean enableCustomTokenReq;

    @Schema(description = "令牌请求配置")
    private RequestConfigRequestDto tokenReqCfg;

    @Schema(description = "是否启用自定义用户信息请求")
    private Boolean enableCustomUserInfoReq;

    @Schema(description = "用户信息请求配置")
    private Map<String, RequestConfigRequestDto> userInfoReqCfg;
}
