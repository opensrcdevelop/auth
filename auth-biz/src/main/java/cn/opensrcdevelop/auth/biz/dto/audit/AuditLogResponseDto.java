package cn.opensrcdevelop.auth.biz.dto.audit;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

@Schema(description = "审计日志响应")
@Data
@Builder
public class AuditLogResponseDto {

    @Schema(description = "日志 ID")
    private String id;

    @Schema(description = "请求 ID")
    private String requestId;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "用户 ID")
    private String userId;

    @Schema(description = "操作类型")
    private Integer type;

    @Schema(description = "资源 ID")
    private String resourceId;

    @Schema(description = "操作详情")
    private String detail;

    @Schema(description = "操作时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime time;

    @Schema(description = "操作结果")
    private Boolean result;

    @Schema(description = "IP")
    private String ip;

    @Schema(description = "IP 归属地")
    private String ipRegion;

    @Schema(description = "操作系统类型")
    private String osType;

    @Schema(description = "浏览器类型")
    private String browserType;

    @Schema(description = "设备类型")
    private String deviceType;
}
