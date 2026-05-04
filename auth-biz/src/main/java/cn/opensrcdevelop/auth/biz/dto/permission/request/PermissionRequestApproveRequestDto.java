package cn.opensrcdevelop.auth.biz.dto.permission.request;

import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import java.util.List;
import lombok.Data;

@Schema(description = "批准或拒绝权限申请请求")
@Data
public class PermissionRequestApproveRequestDto {

    @Schema(description = "是否批准")
    @NotNull
    private Boolean approve;

    @Schema(description = "权限申请ID")
    @NotBlankStr
    private String requestId;

    @Schema(description = "要批准或拒绝的明细ID列表（不填则批准或拒绝所有 PENDING 状态的项）")
    private List<@NotBlank String> itemIds;

    @Schema(description = "限制条件ID列表")
    private List<@NotBlank String> expressionIds;

    @Schema(description = "拒绝原因")
    private String rejectReason;
}
