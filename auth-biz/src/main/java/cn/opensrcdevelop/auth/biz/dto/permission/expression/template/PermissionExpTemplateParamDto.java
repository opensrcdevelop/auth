package cn.opensrcdevelop.auth.biz.dto.permission.expression.template;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import java.io.Serial;
import java.io.Serializable;
import lombok.Data;

@Schema(description = "权限表达式模版参数")
@Data
public class PermissionExpTemplateParamDto implements Serializable {

    @Serial
    private static final long serialVersionUID = 8686253181178087166L;

    @Schema(description = "参数标识")
    @NotBlank
    private String code;

    @Schema(description = "参数值")
    private Object value;
}
