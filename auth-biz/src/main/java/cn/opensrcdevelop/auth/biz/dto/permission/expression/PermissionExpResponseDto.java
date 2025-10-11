package cn.opensrcdevelop.auth.biz.dto.permission.expression;

import cn.opensrcdevelop.auth.biz.dto.permission.expression.template.PermissionExpTemplateParamDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@Schema(description = "权限表达式响应")
@Data
public class PermissionExpResponseDto implements Serializable {

    @Serial
    private static final long serialVersionUID = 5960756665250441854L;

    @Schema(description = "表达式 ID")
    private String id;

    @Schema(description = "表达式名称")
    private String name;

    @Schema(description = "表达式")
    private String expression;

    @Schema(description = "是否使用模板")
    private Boolean useTemplate;

    @Schema(description = "模板ID")
    private String templateId;

    @Schema(description = "模板参数")
    private List< PermissionExpTemplateParamDto> templateParams;

    @Schema(description = "描述")
    private String desc;
}
