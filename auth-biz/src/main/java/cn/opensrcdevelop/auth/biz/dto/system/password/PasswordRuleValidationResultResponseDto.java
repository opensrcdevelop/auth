package cn.opensrcdevelop.auth.biz.dto.system.password;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;

@Schema(description = "密码规则校验结果")
@Builder
@Data
public class PasswordRuleValidationResultResponseDto {

    @Schema(description = "密码规则")
    private String rule;

    @Schema(description = "是否通过校验")
    private Boolean valid;
}
