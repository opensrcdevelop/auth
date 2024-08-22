package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.User;
import cn.opensrcdevelop.auth.biz.entity.UserGroup;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

public interface UserGroupRepository {

    List<UserGroup> searchUserGroups(String userId);

    void searchGroupUsers(IPage<User> page, String userGroupId, String keyword);
}
