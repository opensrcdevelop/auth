package cn.opensrcdevelop.auth.biz.repository.user.attr.impl;

import cn.opensrcdevelop.auth.biz.entity.user.attr.UserAttr;
import cn.opensrcdevelop.auth.biz.mapper.user.attr.UserAttrMapper;
import cn.opensrcdevelop.auth.biz.repository.user.attr.UserAttrRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
@RequiredArgsConstructor
public class UserAttrRepositoryImpl implements UserAttrRepository {

    private final UserAttrMapper userAttrMapper;

    /**
     * 检索用户属性
     *
     * @param userId 用户 ID
     * @return 用户属性
     */
    @Override
    public List<UserAttr> searchUserAttrs(String userId) {
        return userAttrMapper.searchUserAttrs(userId);
    }

    /**
     * 获取最大显示顺序
     *
     * @return 最大显示顺序
     */
    @Override
    public Integer getMaxDisplaySeq() {
        return userAttrMapper.getMaxDisplaySeq();
    }
}
