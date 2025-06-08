package cn.opensrcdevelop.auth.biz.dto.auth;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "Totp 校验请求")
@Data
public class TotpCodeCheckResponseDto {

    @Schema(description = "校验结果")
    private Boolean valid;
}
