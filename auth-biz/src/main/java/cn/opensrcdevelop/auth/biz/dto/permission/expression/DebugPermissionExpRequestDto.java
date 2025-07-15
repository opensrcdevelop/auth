package cn.opensrcdevelop.auth.biz.dto.permission.expression;

import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamDto;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Schema(description = "调试权限表达式请求")
@Data
public class DebugPermissionExpRequestDto {

    @Schema(description = "权限表达式模板ID")
    @NotBlank(groups = { UseTemplate.class })
    private String templateId;

    @Schema(description = "是否使用模板")
    @NotNull
    private Boolean useTemplate;

    @Schema(description = "权限表达式ID")
    @NotBlank(groups = { NotUseTemplate.class })
    private String expressionId;

    @Schema(description = "模板参数")
    private List<@Valid PermissionExpTemplateParamDto> templateParams;

    @Schema(description = "上下文")
    private Map<String, Object> context;

    public interface UseTemplate {}
    public interface NotUseTemplate {}
}
