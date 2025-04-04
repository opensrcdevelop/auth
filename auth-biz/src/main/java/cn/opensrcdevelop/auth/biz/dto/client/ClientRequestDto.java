package cn.opensrcdevelop.auth.biz.dto.client;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

@Schema(description = "创建 / 更新客户端请求")
@Data
public class ClientRequestDto {

    @Schema(description = "客户端ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "客户端名称")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "描述")
    private String desc;

    @Schema(description = "授权模式", examples = {"authorization_code", "refresh_token", "client_credentials", "password"})
    @NotEmpty(groups = { ValidationGroups.Operation.INSERT.class })
    @EnumValue({"authorization_code", "refresh_token", "client_credentials", "password"})
    private List<String> grantTypes;

    @Schema(description = "认证方式", examples = {"client_secret_basic", "client_secret_post"})
    @NotEmpty(groups = { ValidationGroups.Operation.INSERT.class })
    @EnumValue({"client_secret_basic", "client_secret_post", "client_secret_jwt", "private_key_jwt", "none"})
    private List<String> authenticationMethods;

    @Schema(description = "回调地址")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String redirectUri;

    @Schema(description = "领域", examples = { "openid", "profile", "phone", "email"})
    private List<String> scopes;

    @Schema(description = "是否需要用户同意授权")
    private Boolean requireAuthorizationConsent;

    @Schema(description = "授权码存活时间（分钟）")
    @Min(1)
    @Max(Integer.MAX_VALUE)
    private Integer authorizationCodeTimeToLive;

    @Schema(description = "访问令牌存活时间（分钟）")
    @Min(1)
    @Max(Integer.MAX_VALUE)
    private Integer accessTokenTimeToLive;

    @Schema(description = "刷新令牌存活时间（分钟）")
    @Min(1)
    @Max(Integer.MAX_VALUE)
    private Integer refreshTokenTimeToLive;
}
