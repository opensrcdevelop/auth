package cn.opensrcdevelop.auth.biz.dto.permission;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Schema(description = "验证权限请求")
@Data
public class VerifyPermissionsRequestDto {

    @Schema(description = "权限列表")
    @NotEmpty
    private List<@NotBlank String> permissions;

    @Schema(description = "上下文")
    private Map<String, Object> context;
}
