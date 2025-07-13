package cn.opensrcdevelop.auth.biz.dto.permission;

import cn.opensrcdevelop.auth.biz.dto.auth.AuthorizeRecordResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.expression.PermissionExpResponseDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

@Schema(description = "权限响应")
@Data
public class PermissionResponseDto {

    @Schema(description = "权限ID")
    private String permissionId;

    @Schema(description = "权限标识")
    private String permissionCode;

    @Schema(description = "权限名称")
    private String permissionName;

    @Schema(description = "权限描述")
    private String permissionDesc;

    @Schema(description = "资源组ID")
    private String resourceGroupId;

    @Schema(description = "资源组标识")
    private String resourceGroupCode;

    @Schema(description = "资源组名称")
    private String resourceGroupName;

    @Schema(description = "资源ID")
    private String resourceId;

    @Schema(description = "资源名称")
    private String resourceName;

    @Schema(description = "资源标识")
    private String resourceCode;

    @Schema(description = "限定条件集合")
    private List<PermissionExpResponseDto> conditions;

    @Schema(description = "被授权主体")
    private String principal;

    @Schema(description = "主体ID")
    private String principalId;

    @Schema(description = "主体类型")
    private String principalType;

    @Schema(description = "主体类型显示名称")
    private String principalTypeDisplayName;

    @Schema(description = "授权ID")
    private String authorizeId;

    @Schema(description = "授权记录")
    private List<AuthorizeRecordResponseDto> authorizeRecords;

    @Schema(description = "优先级")
    private Integer priority;
}
