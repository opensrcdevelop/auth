package cn.opensrcdevelop.auth.biz.dto.client.oidc;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "OIDC claim 响应")
@Data
public class OidcClaimResponseDto {

    @Schema(description = "claim ID")
    private String id;

    @Schema(description = "claim 名")
    private String name;

    @Schema(description = "用户属性 ID")
    private String userAttrId;
}
