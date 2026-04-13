package cn.opensrcdevelop.auth.biz.dto.permission;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.List;
import lombok.Data;

@Schema(description = "权限树节点（资源组级别）")
@Data
public class PermissionTreeNodeDto implements Serializable {

    @Schema(description = "资源组ID")
    private String resourceGroupId;

    @Schema(description = "资源组名称")
    private String resourceGroupName;

    @Schema(description = "资源组标识")
    private String resourceGroupCode;

    @Schema(description = "资源列表")
    private List<PermissionTreeResourceDto> resources;
}
