package cn.opensrcdevelop.auth.biz.repository;

import cn.opensrcdevelop.auth.biz.entity.PasswordPolicy;

public interface PasswordPolicyRepository {

    PasswordPolicy getMatchedPasswordPolicy(String userId);
}
