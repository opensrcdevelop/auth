package cn.opensrcdevelop.auth.biz.dto.system.password;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Data;

@Schema(description = "密码到期提醒日志响应")
@Builder
@Data
public class UpdatePasswordRemindLogResponseDto {

    @Schema(description = "用户ID")
    private String userId;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "密码策略ID")
    private String policyId;

    @Schema(description = "密码策略名称")
    private String policyName;

    @Schema(description = "提醒方式")
    private String remindMethod;

    @Schema(description = "提醒时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSSSSS)
    private LocalDateTime remindTime;

    @Schema(description = "是否成功")
    private Boolean success;
}
