package cn.opensrcdevelop.auth.biz.mapper.permission;

import cn.opensrcdevelop.auth.biz.entity.auth.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.permission.Permission;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface PermissionMapper extends BaseMapper<Permission> {

    IPage<AuthorizeRecord> searchUserPermissions(@Param("page") IPage<AuthorizeRecord> page,
                                                 @Param("userId") String userId,
                                                 @Param("resourceGroupCode") String resourceGroupCode,
                                                 @Param("resourceGroupNameSearchKeyword") String resourceGroupNameSearchKeyword,
                                                 @Param("resourceNameSearchKeyword") String resourceNameSearchKeyword,
                                                 @Param("permissionNameSearchKeyword") String permissionNameSearchKeyword,
                                                 @Param("permissionCodeSearchKeyword") String permissionCodeSearchKeyword);

    IPage<AuthorizeRecord> searchUserGroupPermissions(@Param("page") IPage<AuthorizeRecord> page,
                                                      @Param("userGroupId") String userGroupId,
                                                      @Param("resourceGroupNameSearchKeyword") String resourceGroupNameSearchKeyword,
                                                      @Param("resourceNameSearchKeyword") String resourceNameSearchKeyword,
                                                      @Param("permissionNameSearchKeyword") String permissionNameSearchKeyword,
                                                      @Param("permissionCodeSearchKeyword") String permissionCodeSearchKeyword);

    IPage<AuthorizeRecord> searchRolePermissions(@Param("page") IPage<AuthorizeRecord> page,
                                                 @Param("roleId") String roleId,
                                                 @Param("resourceGroupNameSearchKeyword") String resourceGroupNameSearchKeyword,
                                                 @Param("resourceNameSearchKeyword") String resourceNameSearchKeyword,
                                                 @Param("permissionNameSearchKeyword") String permissionNameSearchKeyword,
                                                 @Param("permissionCodeSearchKeyword") String permissionCodeSearchKeyword);

    List<AuthorizeRecord> searchPermissionAuthorizeRecords(@Param("permissionId") String permissionId, @Param("keyword") String keyword);

    List<AuthorizeRecord> searchExpPermissions(@Param("expressionId") String expressionId);

    Permission getPermission(@Param("permissionId") String permissionId);
}
