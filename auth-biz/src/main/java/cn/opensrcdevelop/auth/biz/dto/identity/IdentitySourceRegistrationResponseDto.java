package cn.opensrcdevelop.auth.biz.dto.identity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;

@Schema(description = "身份源响应")
@Data
@Builder
public class IdentitySourceRegistrationResponseDto {

    @Schema(description = "身份源ID")
    private String id;

    @Schema(description = "身份源名称")
    private String name;

    @Schema(description = "身份源标识")
    private String code;

    @Schema(description = "logo")
    private String logo;

    @Schema(description = "授权 URI")
    private String authorizationUri;

    @Schema(description = "客户端ID")
    private String clientId;

    @Schema(description = "客户端密钥")
    private String clientSecret;

    @Schema(description = "客户端认证方式")
    private String clientAuthenticationMethod;

    @Schema(description = "授权类型")
    private String authorizationGrantType;

    @Schema(description = "是否启用")
    private Boolean enabled;

    @Schema(description = "提供商")
    private IdentitySourceProviderResponseDto provider;

    @Schema(description = "额外参数")
    private String additionalParams;

    @Schema(description = "是否绑定用户")
    private Boolean isBind;

    @Schema(description = "绑定的用户名")
    private String bindUsername;
}
