package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "重置密码请求")
@Data
public class ResetPwdRequestDto {

    @Schema(description = "新密码")
    @NotBlank
    private String newPwd;

    @Schema(description = "重置密码 token")
    @NotBlank
    private String resetPwdToken;
}
