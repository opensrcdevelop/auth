package cn.opensrcdevelop.auth.biz.dto.system.password;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "检查密码强度请求")
@Data
public class CheckPasswordStrengthRequestDto {

    @Schema(description = "密码")
    @NotNull
    private String password;

    @Schema(description = "用户ID")
    @NotBlank
    private String userId;
}
