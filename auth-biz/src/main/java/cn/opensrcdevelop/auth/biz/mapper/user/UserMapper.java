package cn.opensrcdevelop.auth.biz.mapper.user;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

@Mapper
public interface UserMapper extends BaseMapper<User> {

    List<User> searchUsers(@Param(Constants.WRAPPER) QueryWrapper<User> wrapper, @Param("limit") int limit,
            @Param("offset") int offset);

    long countUsers(@Param(Constants.WRAPPER) QueryWrapper<User> wrapper);
}
