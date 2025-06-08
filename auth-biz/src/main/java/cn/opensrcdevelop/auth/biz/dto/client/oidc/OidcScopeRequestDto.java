package cn.opensrcdevelop.auth.biz.dto.client.oidc;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.List;

@Schema(description = "创建 / 更新 OIDC scope 请求")
@Data
public class OidcScopeRequestDto {

    @Schema(description = "scope ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "scope 名")
    @NotBlank
    private String name;

    @Schema(description = "claim ID 集合")
    private List<String> claims;
}
