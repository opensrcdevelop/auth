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
     * @param limit   分页大小
     * @param offset  分页偏移量
     */
    @Override
    public List<User> searchUsers(QueryWrapper<User> wrapper, int limit, int offset) {
        return userMapper.searchUsers(wrapper, limit, offset);
    }

    /**
     * 查询用户数
     *
     * @param wrapper 条件构造器
     * @return 用户数
     */
    @Override
    public long countUsers(QueryWrapper<User> wrapper) {
        return userMapper.countUsers(wrapper);
    }
}
