package cn.opensrcdevelop.auth.biz.repository.user.group;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

public interface UserGroupRepository {

    List<UserGroup> searchUserGroups(String userId);

    void searchGroupUsers(IPage<User> page, String userGroupId, String keyword);
}
