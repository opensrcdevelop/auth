package cn.opensrcdevelop.tenant.dto;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.LocalDateTime;

@Schema(description = "租户响应")
@Data
public class TenantResponseDto {

    @Schema(description = "租户 ID")
    private String id;

    @Schema(description = "租户名称")
    private String name;

    @Schema(description = "租户标识")
    private String code;

    @Schema(description = "租户描述")
    private String desc;

    @Schema(description = "启用状态")
    private Boolean enabled;

    @Schema(description = "发行方")
    private String issuer;

    @Schema(description = "控制台 url")
    private String consoleUrl;

    @Schema(description = "创建时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime createTime;
}
