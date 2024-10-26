package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.AuthorizeRecord;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

public interface PermissionRepository {

    void searchUserPermissions(IPage<AuthorizeRecord> page, String userId, String resourceGroupCode, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void searchUserGroupPermissions(IPage<AuthorizeRecord> page, String userGroupId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    void searchRolePermissions(IPage<AuthorizeRecord> page, String roleId, String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword, String permissionCodeSearchKeyword);

    List<AuthorizeRecord> searchPermissionAuthorizeRecords(String permissionId, String keyword);

    List<AuthorizeRecord> searchExpPermission(String expressionId);
}
