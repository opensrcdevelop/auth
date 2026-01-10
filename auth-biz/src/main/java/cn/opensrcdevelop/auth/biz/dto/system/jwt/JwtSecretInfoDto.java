package cn.opensrcdevelop.auth.biz.dto.system.jwt;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import lombok.Data;

@Schema(description = "JWT 密钥信息")
@Data
public class JwtSecretInfoDto {

    @Schema(description = "密钥ID")
    private String kid;

    @Schema(description = "密钥算法")
    private String alg;

    @Schema(description = "创建时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime createTime;

    @Schema(description = "过期时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime expireTime;
}
