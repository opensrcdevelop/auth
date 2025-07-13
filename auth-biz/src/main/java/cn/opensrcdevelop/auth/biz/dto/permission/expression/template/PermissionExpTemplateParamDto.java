package cn.opensrcdevelop.auth.biz.dto.permission.expression.template;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "权限表达式模版参数")
@Data
public class PermissionExpTemplateParamDto {

    @Schema(description = "参数标识")
    @NotBlank
    private String code;

    @Schema(description = "参数值")
    private Object value;
}
