package cn.opensrcdevelop.auth.biz.repository.system.password.impl;

import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicy;
import cn.opensrcdevelop.auth.biz.mapper.system.password.PasswordPolicyMapper;
import cn.opensrcdevelop.auth.biz.repository.system.password.PasswordPolicyRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class PasswordPolicyRepositoryImpl implements PasswordPolicyRepository {

    private final PasswordPolicyMapper passwordPolicyMapper;

    /**
     * 获取用户匹配的密码策略
     *
     * @param userId
     *            用户ID
     * @return 用户匹配的密码策略
     */
    @Override
    public PasswordPolicy getMatchedPasswordPolicy(String userId) {
        return passwordPolicyMapper.getMatchedPasswordPolicy(userId);
    }
}
