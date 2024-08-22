package cn.opensrcdevelop.auth.biz.repository.impl;

import cn.opensrcdevelop.auth.biz.entity.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.mapper.PermissionMapper;
import cn.opensrcdevelop.auth.biz.repository.PermissionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class PermissionRepositoryImpl implements PermissionRepository {

    private final PermissionMapper permissionMapper;

    /**
     * 检索用户权限
     *
     * @param userId            用户ID
     * @param resourceGroupCode 资源组code
     * @return 用户权限
     */
    @Override
    public List<AuthorizeRecord> searchUserPermissions(String userId, String resourceGroupCode) {
        return permissionMapper.searchUserPermissions(userId, resourceGroupCode);
    }

    /**
     * 检索用户组权限
     *
     * @param userGroupId 用户组ID
     * @return 用户组权限
     */
    @Override
    public List<AuthorizeRecord> searchUserGroupPermissions(String userGroupId) {
        return permissionMapper.searchUserGroupPermissions(userGroupId);
    }

    /**
     * 检索角色权限
     *
     * @param roleId 角色ID
     * @return 角色权限集合
     */
    @Override
    public List<AuthorizeRecord> searchRolePermissions(String roleId) {
        return permissionMapper.searchRolePermissions(roleId);
    }

    /**
     * 检索权限授权记录
     *
     * @param permissionId 权限ID
     * @param keyword      被授权主体关键字
     * @return 权限授权记录
     */
    @Override
    public List<AuthorizeRecord> searchPermissionAuthorizeRecords(String permissionId, String keyword) {
        return permissionMapper.searchPermissionAuthorizeRecords(permissionId, keyword);
    }

    /**
     * 检索权限表达式关联的权限
     *
     * @param expressionId 表达式ID
     * @return 权限表达式关联的权限
     */
    @Override
    public List<AuthorizeRecord> searchExpPermission(String expressionId) {
        return permissionMapper.searchExpPermissions(expressionId);
    }
}
