package cn.opensrcdevelop.auth.biz.service.role;

import cn.opensrcdevelop.auth.biz.dto.permission.PermissionResponseDto;
import cn.opensrcdevelop.auth.biz.dto.role.RoleMappingRequestDto;
import cn.opensrcdevelop.auth.biz.dto.role.RoleRequestDto;
import cn.opensrcdevelop.auth.biz.dto.role.RoleResponseDto;
import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.common.response.PageData;
import com.baomidou.mybatisplus.extension.service.IService;
import java.util.List;

public interface RoleService extends IService<Role> {

    void createRole(RoleRequestDto requestDto);

    void createUserRoleMapping(RoleMappingRequestDto requestDto);

    List<Role> getUserRoles(String userId);

    void removeUserRoleMapping(RoleMappingRequestDto requestDto);

    PageData<RoleResponseDto> listRoles(int page, int size, String keyword);

    void removeUserRoleMapping(String principalId);

    PageData<RoleResponseDto> getRolePrincipals(int page, int size, String roleId, String keyword);

    RoleResponseDto detail(String roleId);

    void updateRole(RoleRequestDto requestDto);

    void removeRole(String roleId);

    PageData<PermissionResponseDto> getPermissions(int page, int size, String roleId,
            String resourceGroupNameSearchKeyword, String resourceNameSearchKeyword, String permissionNameSearchKeyword,
            String permissionCodeSearchKeyword);
}
