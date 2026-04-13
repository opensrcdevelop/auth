package cn.opensrcdevelop.auth.biz.dto.permission;

import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.util.List;
import lombok.Data;

@Schema(description = "权限树资源节点")
@Data
public class PermissionTreeResourceDto implements Serializable {

    @Schema(description = "资源ID")
    private String resourceId;

    @Schema(description = "资源名称")
    private String resourceName;

    @Schema(description = "资源标识")
    private String resourceCode;

    @Schema(description = "权限列表")
    private List<PermissionTreePermissionDto> permissions;
}
