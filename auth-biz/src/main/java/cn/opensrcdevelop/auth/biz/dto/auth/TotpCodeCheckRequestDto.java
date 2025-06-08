package cn.opensrcdevelop.auth.biz.dto.auth;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "Totp 校验请求")
@Data
public class TotpCodeCheckRequestDto {

    @Schema(description = "一次性密码")
    @NotNull
    private Long code;
}
