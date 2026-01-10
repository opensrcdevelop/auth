package cn.opensrcdevelop.auth.biz.repository.user;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import java.util.List;

public interface UserRepository {

    List<User> searchUsers(QueryWrapper<User> wrapper, int limit, int offset);

    long countUsers(QueryWrapper<User> wrapper);
}
