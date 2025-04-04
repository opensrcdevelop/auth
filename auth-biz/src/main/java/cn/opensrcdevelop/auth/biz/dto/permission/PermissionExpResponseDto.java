package cn.opensrcdevelop.auth.biz.dto.permission;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

@Schema(description = "权限表达式响应")
@Data
public class PermissionExpResponseDto {

    @Schema(description = "表达式 ID")
    private String id;

    @Schema(description = "表达式名称")
    private String name;

    @Schema(description = "表达式")
    private String expression;

    @Schema(description = "表达式描述")
    private String desc;

    @Schema(description = "权限")
    private List<PermissionResponseDto> permissions;
}
