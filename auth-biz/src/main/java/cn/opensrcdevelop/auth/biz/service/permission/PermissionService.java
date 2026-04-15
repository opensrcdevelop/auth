package cn.opensrcdevelop.auth.biz.service.permission;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionTreeNodeDto;
import cn.opensrcdevelop.auth.biz.dto.permission.VerifyPermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.permission.VerifyPermissionsRequestDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

public interface PermissionService extends IService<Permission> {

    void createPermission(PermissionRequestDto requestDto);

    List<PermissionResponseDto> getCurrentUserPermissions();

    void getUserPermissions(IPage<AuthorizeRecord> page, String userId, List<String> dynamicUserGroupIds,
            String resourceGroupCode, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword,
            String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void getUserGroupPermissions(IPage<AuthorizeRecord> page, String userGroupId, String resourceGroupNameSearchKeyword,
            String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void getRolePermissions(IPage<AuthorizeRecord> page, String roleId, String resourceGroupNameSearchKeyword,
            String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void getResourcePermissions(IPage<Permission> page, String resourceId, String keyword);

    PermissionResponseDto detail(String permissionId, String keyword);

    void removeResourcePermissions(List<String> resourceIds);

    void removePermission(String permissionId);

    void updatePermission(PermissionRequestDto requestDto);

    List<AuthorizeRecord> getExpPermissions(String expressionId);

    List<VerifyPermissionResponseDto> verifyPermissions(VerifyPermissionsRequestDto requestDto);

    /**
     * 获取可申请的权限树
     *
     * @param ownedPermissionIds
     *            用户已拥有的权限ID列表（用于标记 alreadyGranted）
     * @return 权限树（按资源组 -> 资源 -> 权限 三层结构）
     */
    List<PermissionTreeNodeDto> getAvailablePermissionTree(List<String> ownedPermissionIds);
}
