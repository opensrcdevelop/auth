package cn.opensrcdevelop.auth.biz.mapper.role;

import cn.opensrcdevelop.auth.biz.entity.role.Role;
import cn.opensrcdevelop.auth.biz.entity.role.RoleMapping;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface RoleMapper extends BaseMapper<Role> {

    List<RoleMapping> searchUserRoles(@Param("userId") String userId);

    IPage<RoleMapping> searchRolePrincipals(@Param("page") IPage<RoleMapping> page,  @Param("roleId") String roleId, @Param("keyword") String keyword);
}
