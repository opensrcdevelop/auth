package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.AuthorizeRecord;

import java.util.List;

public interface PermissionRepository {

    List<AuthorizeRecord> searchUserPermissions(String userId, String resourceGroupCode);

    List<AuthorizeRecord> searchUserGroupPermissions(String userGroupId);

    List<AuthorizeRecord> searchRolePermissions(String roleId);

    List<AuthorizeRecord> searchPermissionAuthorizeRecords(String permissionId, String keyword);

    List<AuthorizeRecord> searchExpPermission(String expressionId);
}
