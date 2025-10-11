package cn.opensrcdevelop.auth.biz.dto.permission.expression.template;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Schema(description = "权限表达式模版响应")
@Setter
@Getter
@Builder
public class PermissionExpTemplateResponseDto {

    @Schema(description = "模版ID")
    private String id;

    @Schema(description = "模版名称")
    private String name;

    @Schema(description = "表达式")
    private String expression;

    @Schema(description = "参数配置")
    private List<PermissionExpTemplateParamConfigDto> paramConfigs;

    @Schema(description = "描述")
    private String desc;
}
