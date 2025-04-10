package cn.opensrcdevelop.auth.biz.repository.role;

import cn.opensrcdevelop.auth.biz.entity.role.RoleMapping;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

public interface RoleRepository {

    List<RoleMapping> searchUserRoles(String userId);

    void searchRolePrincipals(IPage<RoleMapping> page, String roleId, String keyword);
}
