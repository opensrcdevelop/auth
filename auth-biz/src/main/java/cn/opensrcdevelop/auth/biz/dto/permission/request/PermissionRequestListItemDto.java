package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * 权限申请记录列表项 DTO
 */
@Data
@Schema(description = "权限申请记录列表项")
public class PermissionRequestListItemDto {

    @Schema(description = "申请ID")
    private String requestId;

    @Schema(description = "状态（PENDING/APPROVED/REJECTED/AUTO_APPROVED）")
    private String status;

    @Schema(description = "申请时间")
    private LocalDateTime requestTime;

    @Schema(description = "申请理由")
    private String reason;

    @Schema(description = "拒绝理由（若有）")
    private String rejectReason;
}
