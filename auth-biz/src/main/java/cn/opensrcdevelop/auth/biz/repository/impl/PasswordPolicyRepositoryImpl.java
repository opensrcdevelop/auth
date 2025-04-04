package cn.opensrcdevelop.auth.biz.repository.impl;

import cn.opensrcdevelop.auth.biz.entity.PasswordPolicy;
import cn.opensrcdevelop.auth.biz.mapper.PasswordPolicyMapper;
import cn.opensrcdevelop.auth.biz.repository.PasswordPolicyRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class PasswordPolicyRepositoryImpl implements PasswordPolicyRepository {

    private final PasswordPolicyMapper passwordPolicyMapper;

    /**
     * 获取用户匹配的密码策略
     *
     * @param userId 用户ID
     * @return 用户匹配的密码策略
     */
    @Override
    public PasswordPolicy getMatchedPasswordPolicy(String userId) {
        return passwordPolicyMapper.getMatchedPasswordPolicy(userId);
    }
}
