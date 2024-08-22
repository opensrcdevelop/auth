package cn.opensrcdevelop.auth.biz.repository.impl;

import cn.opensrcdevelop.auth.biz.entity.Role;
import cn.opensrcdevelop.auth.biz.entity.RoleMapping;
import cn.opensrcdevelop.auth.biz.mapper.RoleMapper;
import cn.opensrcdevelop.auth.biz.repository.RoleRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class RoleRepositoryImpl implements RoleRepository {

    private final RoleMapper roleMapper;

    /**
     * 检索用户角色
     *
     * @param userId 用户ID
     * @return 用户角色
     */
    @Override
    public List<RoleMapping> searchUserRoles(String userId) {
        return roleMapper.searchUserRoles(userId);
    }

    /**
     * 检索角色主体
     *
     * @param page 分页对象
     * @param roleId 角色ID
     * @param keyword 用户名 / 用户组名称检索关键字
     */
    @Override
    public void searchRolePrincipals(IPage<RoleMapping> page, String roleId, String keyword) {
        roleMapper.searchRolePrincipals(page, roleId, keyword);
    }
}
