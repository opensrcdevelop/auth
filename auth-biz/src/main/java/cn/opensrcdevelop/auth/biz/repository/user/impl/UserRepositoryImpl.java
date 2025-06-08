package cn.opensrcdevelop.auth.biz.repository.user.impl;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mapper.user.UserMapper;
import cn.opensrcdevelop.auth.biz.repository.user.UserRepository;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class UserRepositoryImpl implements UserRepository {

    private final UserMapper userMapper;

    /**
     * 查询用户
     * <a href="https://baomidou.com/guides/wrapper/#%E4%BD%BF%E7%94%A8-wrapper-%E8%87%AA%E5%AE%9A%E4%B9%89-sql">MP</a>
     *
     * @param wrapper 条件构造器
     */
    @Override
    public List<User> searchUsers(QueryWrapper<User> wrapper) {
        return userMapper.searchUsers(wrapper);
    }
}
