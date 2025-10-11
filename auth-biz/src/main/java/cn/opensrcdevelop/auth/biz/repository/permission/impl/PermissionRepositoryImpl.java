package cn.opensrcdevelop.auth.biz.repository.permission.impl;

import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import cn.opensrcdevelop.auth.biz.mapper.permission.PermissionMapper;
import cn.opensrcdevelop.auth.biz.repository.permission.PermissionRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
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
     * @param page                           分页对象
     * @param userId                         用户ID
     * @param resourceGroupCode              资源组标识
     * @param resourceGroupNameSearchKeyword 资源组名称搜索关键字
     * @param resourceNameSearchKeyword      资源名称搜索关键字
     * @param permissionNameSearchKeyword    权限名称搜索关键字
     * @param permissionCodeSearchKeyword    权限标识搜索关键字
     */
    @Override
    public void searchUserPermissions(IPage<AuthorizeRecord> page,
                                      String userId,
                                      String resourceGroupCode,
                                      String resourceGroupNameSearchKeyword,
                                      String resourceNameSearchKeyword,
                                      String permissionNameSearchKeyword,
                                      String permissionCodeSearchKeyword) {
        permissionMapper.searchUserPermissions(page, userId, resourceGroupCode, resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);
    }

    /**
     * 检索用户组权限
     *
     * @param page                           分页对象
     * @param userGroupId                    用户组ID
     * @param resourceGroupNameSearchKeyword 资源组名称搜索关键字
     * @param resourceNameSearchKeyword      资源名称搜索关键字
     * @param permissionNameSearchKeyword    权限名称搜索关键字
     * @param permissionCodeSearchKeyword    权限标识搜索关键字
     */
    @Override
    public void searchUserGroupPermissions(IPage<AuthorizeRecord> page,
                                           String userGroupId,
                                           String resourceGroupNameSearchKeyword,
                                           String resourceNameSearchKeyword,
                                           String permissionNameSearchKeyword,
                                           String permissionCodeSearchKeyword) {
        permissionMapper.searchUserGroupPermissions(page, userGroupId, resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);
    }

    /**
     * 检索角色权限
     *
     * @param page                           分页对象
     * @param roleId                         角色ID
     * @param resourceGroupNameSearchKeyword 资源组名称搜索关键字
     * @param resourceNameSearchKeyword      资源名称搜索关键字
     * @param permissionNameSearchKeyword    权限名称搜索关键字
     * @param permissionCodeSearchKeyword    权限标识搜索关键字
     */
    @Override
    public void searchRolePermissions(IPage<AuthorizeRecord> page,
                                      String roleId,
                                      String resourceGroupNameSearchKeyword,
                                      String resourceNameSearchKeyword,
                                      String permissionNameSearchKeyword,
                                      String permissionCodeSearchKeyword) {
        permissionMapper.searchRolePermissions(page, roleId, resourceGroupNameSearchKeyword, resourceNameSearchKeyword, permissionNameSearchKeyword, permissionCodeSearchKeyword);
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

    /**
     * 获取权限
     *
     * @param permissionId 权限ID
     * @return 权限
     */
    @Override
    public Permission getPermission(String permissionId) {
        return permissionMapper.getPermission(permissionId);
    }
}
