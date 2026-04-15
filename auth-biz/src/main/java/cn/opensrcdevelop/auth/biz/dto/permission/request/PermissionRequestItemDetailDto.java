package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 权限申请明细详情 DTO
 */
@Data
@Schema(description = "权限申请明细详情")
public class PermissionRequestItemDetailDto {

    @Schema(description = "权限ID")
    private String permissionId;

    @Schema(description = "权限名称")
    private String permissionName;

    @Schema(description = "权限代码")
    private String permissionCode;

    @Schema(description = "审批状态（PENDING待审批/APPROVED已批准/REJECTED已拒绝/AUTO_APPROVED自动批准）")
    private String status;

    @Schema(description = "拒绝理由（针对单个权限）")
    private String rejectReason;
}
