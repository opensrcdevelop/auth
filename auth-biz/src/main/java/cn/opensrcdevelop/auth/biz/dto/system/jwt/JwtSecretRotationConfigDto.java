package cn.opensrcdevelop.auth.biz.dto.system.jwt;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "JWT 密钥轮换配置 请求/响应")
@Data
public class JwtSecretRotationConfigDto {

    @Schema(description = "密钥轮换周期")
    @NotNull
    @Min(1)
    @Max(99)
    private Integer rotationPeriod;

    @Schema(description = "密钥轮换周期单位")
    @NotNull
    private String  rotationPeriodUnit;
}
