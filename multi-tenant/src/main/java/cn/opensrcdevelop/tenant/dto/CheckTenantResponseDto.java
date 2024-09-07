package cn.opensrcdevelop.tenant.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
public class CheckTenantResponseDto {

    @Schema(description = "发行方")
    private String issuer;

    @Schema(description = "是否存在")
    private Boolean exists;
}
