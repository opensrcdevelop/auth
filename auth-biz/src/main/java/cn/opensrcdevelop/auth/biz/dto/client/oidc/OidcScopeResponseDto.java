package cn.opensrcdevelop.auth.biz.dto.client.oidc;

import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.Data;

@Schema(description = "OIDC scope 响应")
@Data
public class OidcScopeResponseDto {

    @Schema(description = "scope ID")
    private String id;

    @Schema(description = "scope 名")
    private String name;

    @Schema(description = "claim 集合")
    private List<String> claims;
}
