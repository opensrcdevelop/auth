package cn.opensrcdevelop.auth.biz.repository.user.impl;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.mapper.user.UserMapper;
import cn.opensrcdevelop.auth.biz.repository.user.UserRepository;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class UserRepositoryImpl implements UserRepository {

    private final UserMapper userMapper;

    /**
     * 查询用户
     * <a href="https://baomidou.com/guides/wrapper/#%E4%BD%BF%E7%94%A8-wrapper-%E8%87%AA%E5%AE%9A%E4%B9%89-sql">MP</a>
     *
     * @param page    分页对象
     * @param wrapper 条件构造器
     */
    @Override
    public void searchUsers(IPage<?> page, QueryWrapper<User> wrapper) {
        userMapper.searchUsers(page, wrapper);
    }
}
