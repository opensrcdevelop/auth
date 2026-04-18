package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 权限申请明细响应
 */
@Data
@Schema(description = "权限申请明细响应")
public class PermissionRequestItemResponseDto {

    @Schema(description = "权限ID")
    private String permissionId;

    @Schema(description = "权限名称")
    private String permissionName;

    @Schema(description = "资源ID")
    private String resourceId;

    @Schema(description = "资源名称")
    private String resourceName;

    @Schema(description = "资源组ID")
    private String resourceGroupId;

    @Schema(description = "资源组名称")
    private String resourceGroupName;

    @Schema(description = "审批状态（PENDING 待审批 / APPROVED 已批准 / REJECTED 已拒绝 / AUTO_APPROVED 自动批准）")
    private String status;

    @Schema(description = "拒绝理由（针对单个权限）")
    private String rejectReason;

    @Schema(description = "审批人用户名")
    private String approverUsername;
}
