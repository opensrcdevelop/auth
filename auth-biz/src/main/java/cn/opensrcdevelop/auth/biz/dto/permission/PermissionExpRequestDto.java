package cn.opensrcdevelop.auth.biz.dto.permission;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "创建 / 更新权限表达式请求")
@Data
public class PermissionExpRequestDto {

    @Schema(description = "表达式ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "表达式名称")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "表达式")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String expression;

    @Schema(description = "描述")
    @NotBlankStr
    private String desc;
}
