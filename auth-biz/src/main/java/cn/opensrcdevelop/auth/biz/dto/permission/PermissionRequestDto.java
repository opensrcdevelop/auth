package cn.opensrcdevelop.auth.biz.dto.permission;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "创建 / 更新权限请求")
@Data
public class PermissionRequestDto {

    @Schema(description = "权限ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "权限名")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "权限标识")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String code;

    @Schema(description = "资源ID")
    @NotBlank
    private String resourceId;

    @Schema(description = "描述")
    @NotBlankStr
    private String desc;
}
