package cn.opensrcdevelop.auth.biz.dto.permission;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

@Schema(description = "权限树响应节点")
@Data
public class PermissionTreeNodeResponseDto {

    @Schema(description = "ID")
    private String id;

    @Schema(description = "名称")
    private String name;

    @Schema(description = "标识")
    private String code;

    @Schema(description = "类型：资源组 / 资源 / 权限")
    private String type;

    @Schema(description = "是否自动批准")
    private Boolean autoApprove;

    @Schema(description = "是否审批中")
    private Boolean pending;

    @Schema(description = "是否已拥有")
    private Boolean owned;

    @Schema(description = "子节点")
    private List<PermissionTreeNodeResponseDto> children;
}
