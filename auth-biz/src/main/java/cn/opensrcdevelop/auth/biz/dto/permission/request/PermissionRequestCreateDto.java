package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;
import lombok.Data;

@Schema(description = "权限申请提交请求")
@Data
public class PermissionRequestCreateDto {

    @Schema(description = "申请的权限ID列表（不可为空）")
    @NotEmpty(message = "权限ID列表不能为空")
    private List<String> permissionIds;

    @Schema(description = "申请理由（必填，整个申请共用一个理由）")
    @NotBlank(message = "申请理由不能为空")
    private String reason;
}
