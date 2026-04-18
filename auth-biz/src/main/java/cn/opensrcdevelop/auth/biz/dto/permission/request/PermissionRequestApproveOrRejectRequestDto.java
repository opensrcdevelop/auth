package cn.opensrcdevelop.auth.biz.dto.permission.request;

import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

@Schema(description = "批准或拒绝权限申请请求")
@Data
public class PermissionRequestApproveOrRejectRequestDto {

    @Schema(description = "操作类型（APPROVE：批准 REJECT：拒绝）")
    @EnumValue({"APPROVE", "REJECT"})
    private String type;

    @Schema(description = "要批准或拒绝的申请项ID列表（可选，不填则批准或拒绝所有 PENDING 状态的项）")
    private List<String> itemIds;

    @Schema(description = "限制条件ID列表（可选）")
    private List<String> expressionIds;

    @Schema(description = "优先级（可选，-1最低 3最高）")
    @EnumValue({"-1", "0", "1", "2", "3"})
    private Integer priority;
}
