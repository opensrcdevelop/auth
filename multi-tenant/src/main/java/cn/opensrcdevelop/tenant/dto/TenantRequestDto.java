package cn.opensrcdevelop.tenant.dto;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.AlphaNum;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import java.time.LocalDateTime;
import lombok.Data;

@Schema(description = "创建 / 更新租户请求")
@Data
public class TenantRequestDto {

    @Schema(description = "租户 ID")
    @NotBlank(groups = ValidationGroups.Operation.UPDATE.class)
    private String id;

    @Schema(description = "租户名称")
    @NotBlank(groups = ValidationGroups.Operation.INSERT.class)
    @NotBlankStr(groups = ValidationGroups.Operation.UPDATE.class)
    private String name;

    @Schema(description = "租户标识")
    @NotBlank(groups = ValidationGroups.Operation.INSERT.class)
    @NotBlankStr(groups = ValidationGroups.Operation.UPDATE.class)
    @AlphaNum(allowHyphen = false, allowUnderline = false, onlyLowerCaseLetter = true)
    private String code;

    @Schema(description = "租户描述")
    @NotBlankStr
    private String desc;

    @Schema(description = "启用状态")
    private Boolean enabled;

    @Schema(description = "生效时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime effectiveTime;

    @Schema(description = "失效时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private LocalDateTime expirationTime;
}
