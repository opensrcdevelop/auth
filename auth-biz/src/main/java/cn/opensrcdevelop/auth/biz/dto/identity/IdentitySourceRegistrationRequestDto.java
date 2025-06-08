package cn.opensrcdevelop.auth.biz.dto.identity;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.Map;

@Schema(description = "身份源请求")
@Data
public class IdentitySourceRegistrationRequestDto {

    @Schema(description = "身份源ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "身份源提供商ID")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    private String providerId;

    @Schema(description = "身份源名称")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "身份源标识")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String code;

    @Schema(description = "客户端ID")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String clientId;

    @Schema(description = "客户端密钥")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String clientSecret;

    @Schema(description = "客户端认证方式")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    @EnumValue({ "client_secret_basic", "client_secret_post" })
    private String clientAuthenticationMethod;

    @Schema(description = "授权类型")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    @EnumValue({ "authorization_code" })
    private String authorizationGrantType;

    @Schema(description = "是否启用")
    private Boolean enabled;

    @Schema(description = "额外参数")
    private Map<String, Object> additionalParams;
}
