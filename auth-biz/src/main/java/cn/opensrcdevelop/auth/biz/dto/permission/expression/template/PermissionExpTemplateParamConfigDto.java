package cn.opensrcdevelop.auth.biz.dto.permission.expression.template;

import cn.opensrcdevelop.auth.biz.constants.PermissionExpTemplateParamType;
import cn.opensrcdevelop.common.validation.constraints.AlphaNum;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;

@Schema(description = "权限表达式模版参数配置")
@Data
public class PermissionExpTemplateParamConfigDto {

    @Schema(description = "类型")
    @NotNull
    private PermissionExpTemplateParamType type;

    @Schema(description = "名称")
    @NotBlank
    private String name;

    @Schema(description = "标识")
    @NotBlank
    @AlphaNum(allowHyphen = false)
    private String code;

    @Schema(description = "描述")
    private String desc;

    @Schema(description = "可选项")
    @NotEmpty(groups = { ChoiceType.class })
    private List<@NotBlank String> options;

    @Schema(description = "是否多选")
    @NotNull(groups = { ChoiceType.class })
    private Boolean multiple;

    @Schema(description = "是否必填")
    @NotNull
    private Boolean required;

    @Schema(description = "默认值")
    private Object defaultValue;

    public interface ChoiceType{}
}
