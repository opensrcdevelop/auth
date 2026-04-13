package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Data;

/**
 * 权限申请详情响应 DTO
 */
@Data
@Schema(description = "权限申请详情响应")
public class PermissionRequestDetailDto {

    @Schema(description = "申请ID")
    private String requestId;

    @Schema(description = "申请人ID")
    private String userId;

    @Schema(description = "申请理由")
    private String reason;

    @Schema(description = "申请时间")
    private LocalDateTime requestTime;

    @Schema(description = "申请的权限明细列表")
    private List<PermissionRequestItemDetailDto> items;
}
