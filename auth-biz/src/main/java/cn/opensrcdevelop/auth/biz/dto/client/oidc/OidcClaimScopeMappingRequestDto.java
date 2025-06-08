package cn.opensrcdevelop.auth.biz.dto.client.oidc;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

@Schema(description = "创建 / 更新 OIDC claim scope 映射关系")
@Data
public class OidcClaimScopeMappingRequestDto {

    @Schema(description = "scope ID")
    @NotBlank
    private String scopeId;

    @Schema(description = "claim ID 集合")
    @NotEmpty
    private List<String> claimIds;
}
