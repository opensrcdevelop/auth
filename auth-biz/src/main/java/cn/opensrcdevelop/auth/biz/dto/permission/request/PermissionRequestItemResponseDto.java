package cn.opensrcdevelop.auth.biz.dto.permission.request;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * 权限申请明细响应
 */
@Data
@Schema(description = "权限申请明细响应")
public class PermissionRequestItemResponseDto {

    @Schema(description = "权限申请明细ID")
    private String id;

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

    @Schema(description = "审批人ID")
    private String approverId;

    @Schema(description = "审批人用户名")
    private String approverUsername;

    @Schema(description = "审批时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime approveTime;
}
