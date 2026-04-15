package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "权限申请提交响应")
@Data
public class PermissionRequestResponseDto {

    @Schema(description = "申请ID")
    private String requestId;

    @Schema(description = "自动批准的权限数量")
    private int autoApprovedCount;

    @Schema(description = "待审批的权限数量")
    private int pendingCount;
}
