package cn.opensrcdevelop.auth.biz.dto.user;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import lombok.Data;

@Schema(description = "登录日志响应")
@Data
public class LoginLogResponseDto {

    @Schema(description = "登录ID")
    private String loginId;

    @Schema(description = "客户端ID")
    private String clientId;

    @Schema(description = "客户端名称")
    private String clientName;

    @Schema(description = "登录IP")
    private String loginIp;

    @Schema(description = "登录IP属地")
    private String loginIpRegion;

    @Schema(description = "设备类型")
    private String deviceType;

    @Schema(description = "设备OS")
    private String deviceOs;

    @Schema(description = "浏览器类型")
    private String browserType;

    @Schema(description = "登录时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime loginTime;
}
