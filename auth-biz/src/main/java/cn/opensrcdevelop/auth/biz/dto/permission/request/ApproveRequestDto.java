package cn.opensrcdevelop.auth.biz.dto.permission.request;

import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import io.swagger.v3.oas.annotations.media.Schema;
import java.util.List;
import lombok.Data;

@Schema(description = "批准权限申请请求")
@Data
public class ApproveRequestDto {

    @Schema(description = "要批准的申请项ID列表（可选，不填则批准所有PENDING状态的项）")
    private List<String> itemIds;

    @Schema(description = "限制条件ID列表（可选）")
    private List<String> expressionIds;

    @Schema(description = "优先级（可选，-1最低 3最高）")
    @EnumValue({"-1", "0", "1", "2", "3"})
    private Integer priority;
}
