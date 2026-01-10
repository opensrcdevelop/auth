package cn.opensrcdevelop.auth.biz.dto.client.oidc;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "创建 OIDC claim 请求")
@Data
public class OidcClaimRequestDto {
    @Schema(description = "claim ID")
    @NotBlank(groups = {ValidationGroups.Operation.UPDATE.class})
    public String id;

    @Schema(description = "claim 名")
    @NotBlank
    private String name;

    @Schema(description = "用户属性 ID")
    @NotBlank
    private String userAttrId;
}
