package cn.opensrcdevelop.auth.biz.dto.permission.request;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

@Schema(description = "权限申请响应")
@Data
public class PermissionRequestResponseDto {

    @Schema(description = "申请ID")
    private String requestId;

    @Schema(description = "申请人ID")
    private String userId;

    @Schema(description = "申请理由")
    private String reason;

    @Schema(description = "申请时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime requestTime;

    @Schema(description = "待审批数量")
    private Long pendingCount;

    @Schema(description = "已批准数量")
    private Long approvedCount;

    @Schema(description = "自动批准数量")
    private Long autoApproveCount;

    @Schema(description = "已拒绝数量")
    private Long rejectedCount;

    @Schema(description = "总数量")
    private Long totalCount;

    @Schema(description = "申请的权限明细列表")
    private List<PermissionRequestItemResponseDto> items;
}
