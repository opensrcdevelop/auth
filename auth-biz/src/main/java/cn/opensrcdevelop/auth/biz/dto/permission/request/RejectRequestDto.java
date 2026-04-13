package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import java.util.List;
import lombok.Data;

@Schema(description = "拒绝权限申请请求")
@Data
public class RejectRequestDto {

    @Schema(description = "要拒绝的申请项ID列表（可选，不填则拒绝所有PENDING状态的项）")
    private List<String> itemIds;

    @Schema(description = "拒绝理由（必填）")
    @NotBlank(message = "拒绝理由不能为空")
    private String rejectReason;
}
