package cn.opensrcdevelop.auth.biz.dto.permission.expression;

import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamDto;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;

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
    @NotBlank(groups = { NoneTemplateInsert.class })
    String expression;

    @Schema(description = "是否使用模板")
    @NotNull
    private Boolean useTemplate;

    @Schema(description = "模板ID")
    @NotNull(groups = { UseTemplateInsert.class })
    private String templateId;

    @Schema(description = "模板参数")
    private List<@Valid PermissionExpTemplateParamDto> templateParams;

    @Schema(description = "描述")
    @NotBlankStr
    private String desc;

    public interface NoneTemplateInsert {}
    public interface NoneTemplateUpdate {}
    public interface UseTemplateInsert {}
    public interface UseTemplateUpdate {}
}
