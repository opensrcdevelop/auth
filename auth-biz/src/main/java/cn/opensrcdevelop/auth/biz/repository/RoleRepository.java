package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.Role;
import cn.opensrcdevelop.auth.biz.entity.RoleMapping;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

public interface RoleRepository {

    List<RoleMapping> searchUserRoles(String userId);

    void searchRolePrincipals(IPage<RoleMapping> page, String roleId, String keyword);
}
