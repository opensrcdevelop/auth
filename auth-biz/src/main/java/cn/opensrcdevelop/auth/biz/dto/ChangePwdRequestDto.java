package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "变更密码请求")
@Data
public class ChangePwdRequestDto {

    @Schema(description = "原密码")
    @NotBlank
    private String rawPwd;

    @Schema(description = "新密码")
    @NotBlank
    private String newPwd;
}
