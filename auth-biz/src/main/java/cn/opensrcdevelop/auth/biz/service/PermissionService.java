package cn.opensrcdevelop.auth.biz.service;

import cn.opensrcdevelop.auth.biz.dto.PermissionRequestDto;
import cn.opensrcdevelop.auth.biz.dto.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.entity.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.Permission;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

public interface PermissionService extends IService<Permission> {

    void createPermission(PermissionRequestDto requestDto);

    List<PermissionResponseDto> getCurrentUserPermissions();

    List<AuthorizeRecord> getUserPermissions(String userId, String resourceGroupCode);

    List<AuthorizeRecord> getUserGroupPermissions(String userGroupId);

    List<AuthorizeRecord> getRolePermissions(String roleId);

    void getResourcePermissions(IPage<Permission> page, String resourceId, String keyword);

    PermissionResponseDto detail(String permissionId, String keyword);

    void removeResourcePermissions(List<String> resourceIds);

    void removePermission(String permissionId);

    void updatePermission(PermissionRequestDto requestDto);

    List<AuthorizeRecord> getExpPermissions(String expressionId);
}
