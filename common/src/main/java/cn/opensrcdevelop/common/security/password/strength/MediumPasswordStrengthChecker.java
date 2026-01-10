package cn.opensrcdevelop.common.security.password.strength;

import cn.opensrcdevelop.common.security.password.CharType;
import cn.opensrcdevelop.common.security.password.rule.CharTypePasswordRule;
import cn.opensrcdevelop.common.security.password.rule.LengthPasswordRule;
import cn.opensrcdevelop.common.security.password.rule.PasswordRuleManager;
import java.util.List;

/**
 * 中强度密码检查器
 *
 */
public class MediumPasswordStrengthChecker implements PasswordStrengthChecker {

    @Override
    public Boolean validate(String password) {
        PasswordRuleManager ruleManager = new PasswordRuleManager();
        ruleManager.addRule(new LengthPasswordRule(6, -1));
        ruleManager
                .addRule(new CharTypePasswordRule(List.of(CharType.LETTER, CharType.DIGIT, CharType.SPECIAL_CHAR), 2));
        return ruleManager.validateAll(password);
    }

    @Override
    public String errorMessage() {
        return "请使用至少 6 位字符作为密码，须包含英文、数字与符号中的两种。";
    }
}
