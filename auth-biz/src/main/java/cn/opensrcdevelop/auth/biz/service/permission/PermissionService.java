package cn.opensrcdevelop.auth.biz.service.permission;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface PermissionService extends IService<Permission> {

    void createPermission(PermissionRequestDto requestDto);

    List<PermissionResponseDto> getCurrentUserPermissions();

    void getUserPermissions(IPage<AuthorizeRecord> page, String userId, String resourceGroupCode, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void getUserGroupPermissions(IPage<AuthorizeRecord> page, String userGroupId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void getRolePermissions(IPage<AuthorizeRecord> page, String roleId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void getResourcePermissions(IPage<Permission> page, String resourceId, String keyword);

    PermissionResponseDto detail(String permissionId, String keyword);

    void removeResourcePermissions(List<String> resourceIds);

    void removePermission(String permissionId);

    void updatePermission(PermissionRequestDto requestDto);

    List<AuthorizeRecord> getExpPermissions(String expressionId);
}
