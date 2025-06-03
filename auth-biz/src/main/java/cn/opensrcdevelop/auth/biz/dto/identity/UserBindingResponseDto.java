package cn.opensrcdevelop.auth.biz.dto.identity;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Schema(description = "用户绑定响应")
@Data
@Builder
public class UserBindingResponseDto {

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "用户ID")
    private String userId;

    @Schema(description = "第三方用户唯一标识")
    private String uniqueId;

    @Schema(description = "绑定时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime bindingTime;
}
