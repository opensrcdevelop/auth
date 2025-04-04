package cn.opensrcdevelop.auth.biz.dto.user;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "检查验证码请求")
@Data
public class CheckCodeRequestDto {

    @Schema(description = "用户名 / 手机号 / 邮箱")
    @NotBlank
    private String username;

    @Schema(description = "验证码")
    @NotBlank
    private String code;
}
