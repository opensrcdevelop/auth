package cn.opensrcdevelop.common.security.password.strength;

import cn.opensrcdevelop.common.security.password.rule.LengthPasswordRule;

/**
 * 低强度密码检查器
 *
 */
public class LowPasswordStrengthPasswordChecker implements PasswordStrengthChecker {

    @Override
    public Boolean validate(String password) {
        LengthPasswordRule rule = new LengthPasswordRule(6, -1);
        return rule.validate(password);
    }

    @Override
    public String errorMessage() {
        return "密码长度不能少于 6 位。";
    }
}
