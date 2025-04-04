package cn.opensrcdevelop.auth.biz.dto.role;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "角色响应")
@Data
public class RoleResponseDto {

    @Schema(description = "角色 ID")
    private String id;

    @Schema(description = "角色名称")
    private String name;

    @Schema(description = "角色码")
    private String code;

    @Schema(description = "角色描述")
    private String desc;

    @Schema(description = "角色主体")
    private String principal;

    @Schema(description = "角色主体ID")
    private String principalId;

    @Schema(description = "主体类型")
    private String principalType;

    @Schema(description = "主体类型显示名称")
    private String principalTypeDisplayName;
}
