package cn.opensrcdevelop.auth.biz.dto.permission.expression.template;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.List;

@Schema(description = "权限表达式模版请求")
@Data
public class PermissionExpTemplateRequestDto {

    @Schema(description = "模版ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "模版名称")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "表达式")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String expression;

    @Schema(description = "参数配置")
    private List<@Valid PermissionExpTemplateParamConfigDto> paramConfigs;

    @Schema(description = "描述")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String desc;
}
