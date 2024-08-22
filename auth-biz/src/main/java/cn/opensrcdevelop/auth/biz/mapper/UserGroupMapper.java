package cn.opensrcdevelop.auth.biz.mapper;

import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.entity.UserGroup;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

@Mapper
public interface UserGroupMapper extends BaseMapper<UserGroup> {

    List<UserGroup> searchUserGroups(@Param("userId") String userId);

    IPage<User> searchGroupUsers(@Param("page") IPage<User> page,  @Param("userGroupId") String userGroupId, @Param("keyword") String keyword);
}
