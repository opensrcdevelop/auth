package cn.opensrcdevelop.auth.biz.dto.system.password;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;

import java.util.List;

@Schema(description = "检查密码强度响应")
@Builder
@Data
public class CheckPasswordStrengthResponseDto {

    @Schema(description = "错误消息")
    private String errorMessage;

    @Schema(description = "是否通过校验")
    private Boolean valid;

    @Schema(description = "密码规则校验结果")
    private List<PasswordRuleValidationResultResponseDto> ruleResults;
}
