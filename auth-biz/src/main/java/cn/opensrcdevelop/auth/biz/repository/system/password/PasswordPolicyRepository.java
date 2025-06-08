package cn.opensrcdevelop.auth.biz.repository.system.password;

import cn.opensrcdevelop.auth.biz.entity.system.password.PasswordPolicy;

public interface PasswordPolicyRepository {

    PasswordPolicy getMatchedPasswordPolicy(String userId);
}
