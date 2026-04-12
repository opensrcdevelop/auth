package cn.opensrcdevelop.auth.biz.dto.permission;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import lombok.Data;

@Schema(description = "权限树权限节点")
@Data
public class PermissionTreePermissionDto implements Serializable {

    @Schema(description = "权限ID")
    private String permissionId;

    @Schema(description = "权限名称")
    private String permissionName;

    @Schema(description = "权限标识")
    private String permissionCode;

    @Schema(description = "权限定位符，格式：{resourceGroupCode}:{resourceCode}:{permissionCode}")
    private String permissionLocator;

    @Schema(description = "是否已拥有（前端用此字段决定是否显示申请按钮）")
    private boolean alreadyGranted;
}