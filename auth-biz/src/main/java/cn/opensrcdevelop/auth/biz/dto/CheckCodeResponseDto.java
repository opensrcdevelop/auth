package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
@Schema(description = "验证码校验结果")
public class CheckCodeResponseDto {

    @Schema(description = "校验结果 token")
    private String resultToken;
}
