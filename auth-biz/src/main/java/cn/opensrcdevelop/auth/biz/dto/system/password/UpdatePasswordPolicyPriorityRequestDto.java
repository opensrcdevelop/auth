package cn.opensrcdevelop.auth.biz.dto.system.password;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "更新密码策略优先级请求")
@Data
public class UpdatePasswordPolicyPriorityRequestDto {

    @Schema(description = "密码策略 ID")
    @NotBlank
    private String id;

    @Schema(description = "密码策略优先级")
    @NotNull
    private Integer priority;
}
