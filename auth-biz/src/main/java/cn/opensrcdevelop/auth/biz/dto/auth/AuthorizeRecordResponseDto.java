package cn.opensrcdevelop.auth.biz.dto.auth;

import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Data;

@Schema(description = "授权记录响应")
@Data
public class AuthorizeRecordResponseDto implements Serializable {

    @Serial
    private static final long serialVersionUID = 7022450554605127518L;

    @Schema(description = "授权ID")
    private String authorizeId;

    @Schema(description = "授权时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime authorizeTime;

    @Schema(description = "被授权主体ID")
    private String principalId;

    @Schema(description = "被授权主体")
    private String principal;

    @Schema(description = "主体类型")
    private String principalType;

    @Schema(description = "主体类型显示名称")
    private String principalTypeDisplayName;

    @Schema(description = "限定条件集合")
    private List<PermissionExpResponseDto> conditions;

    @Schema(description = "优先级")
    private Integer priority;
}
