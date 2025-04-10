package cn.opensrcdevelop.auth.biz.repository.user;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;

public interface UserRepository {

    void searchUsers(IPage<?> page, QueryWrapper<User> wrapper);
}
