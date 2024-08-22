package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

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
