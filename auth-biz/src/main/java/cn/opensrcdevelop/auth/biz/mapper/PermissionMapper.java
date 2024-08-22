package cn.opensrcdevelop.auth.biz.mapper;

import cn.opensrcdevelop.auth.biz.entity.AuthorizeRecord;
import cn.opensrcdevelop.auth.biz.entity.Permission;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface PermissionMapper extends BaseMapper<Permission> {

    List<AuthorizeRecord> searchUserPermissions(@Param("userId") String userId, @Param("resourceGroupCode") String resourceGroupCode);

    List<AuthorizeRecord> searchUserGroupPermissions(@Param("userGroupId") String userGroupId);

    List<AuthorizeRecord> searchRolePermissions(@Param("roleId") String roleId);

    List<AuthorizeRecord> searchPermissionAuthorizeRecords(@Param("permissionId") String permissionId, @Param("keyword") String keyword);

    List<AuthorizeRecord> searchExpPermissions(@Param("expressionId") String expressionId);
}
