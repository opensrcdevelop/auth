package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "绑定 / 解绑邮箱请求")
@Data
public class BindOrUnbindEmailRequestDto {

    @Schema(description = "邮箱")
    @NotBlank
    @Email
    private String email;

    @Schema(description = "验证码")
    @NotBlank
    private String code;
}
