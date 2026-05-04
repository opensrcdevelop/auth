package cn.opensrcdevelop.auth.biz.dto.permission.request;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;
import lombok.Data;

@Schema(description = "权限申请提交请求")
@Data
public class PermissionRequestCreateDto {

    @Schema(description = "申请的权限ID列表")
    @NotEmpty
    private List<@NotBlank String> permissionIds;

    @Schema(description = "申请理由")
    @NotBlank
    private String reason;
}
