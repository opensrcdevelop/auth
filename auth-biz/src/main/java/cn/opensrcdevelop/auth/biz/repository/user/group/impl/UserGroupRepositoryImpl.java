package cn.opensrcdevelop.auth.biz.repository.user.group.impl;

import cn.opensrcdevelop.auth.biz.entity.user.User;
import cn.opensrcdevelop.auth.biz.entity.user.group.UserGroup;
import cn.opensrcdevelop.auth.biz.mapper.user.group.UserGroupMapper;
import cn.opensrcdevelop.auth.biz.repository.user.group.UserGroupRepository;
import com.baomidou.mybatisplus.core.metadata.IPage;
import java.util.List;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class UserGroupRepositoryImpl implements UserGroupRepository {

    private final UserGroupMapper userGroupMapper;

    /**
     * 检索用户组
     *
     * @param userId
     *            用户 ID
     * @return 用户组
     */
    @Override
    public List<UserGroup> searchUserGroups(String userId) {
        return userGroupMapper.searchUserGroups(userId);
    }

    /**
     * 检索组内用户
     *
     * @param page
     *            分页对象
     * @param userGroupId
     *            用户组 ID
     * @param keyword
     *            用户名 / 邮箱 / 手机号检索关键字
     */
    @Override
    public void searchGroupUsers(IPage<User> page, String userGroupId, String keyword) {
        userGroupMapper.searchGroupUsers(page, userGroupId, keyword);
    }
}
