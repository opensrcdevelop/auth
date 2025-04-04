package cn.opensrcdevelop.common.security.password.strength;

import cn.opensrcdevelop.common.security.password.CharType;
import cn.opensrcdevelop.common.security.password.PasswordComplexityConfig;
import cn.opensrcdevelop.common.security.password.rule.*;
import io.vavr.Tuple2;
import lombok.RequiredArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * 自定义密码强度检查器
 *
 */
@RequiredArgsConstructor
public class CustomPasswordStrengthChecker implements PasswordStrengthChecker {

    private final PasswordComplexityConfig complexityConfig;
    private final List<String> userInfoList = new ArrayList<>();

    @Override
    public Boolean validate(String password) {
        PasswordRuleManager ruleManager = buildRuleManager();
        return ruleManager.validateAll(password);
    }

    @Override
    public String errorMessage() {
        return "密码不符合要求。";
    }

    /**
     * 获取密码检查结果，返回一个包含检查结果的列表，每个元素是一个元组，包含规则名称和检查结果。
     *
     * @param password 密码
     * @return 检查结果列表
     */
    public List<Tuple2<String, Boolean>> getValidateResult(String password) {
        PasswordRuleManager ruleManager = buildRuleManager();
        return ruleManager.validate(password);
    }

    /**
     * 添加用户信息，用于密码规则中禁止包含用户信息
     *
     * @param userInfo 用户信息
     */
    public void addUserInfo(String userInfo) {
        userInfoList.add(userInfo);
    }

    private PasswordRuleManager buildRuleManager() {
        PasswordRuleManager ruleManager = new PasswordRuleManager();
        // 长度规则
        if (complexityConfig.getMaxLength() != -1 && complexityConfig.getMinLength() > complexityConfig.getMaxLength()) {
            throw new IllegalArgumentException("密码长度规则配置错误，最小长度不能大于最大长度。");
        }
        ruleManager.addRule(new LengthPasswordRule(complexityConfig.getMinLength(), complexityConfig.getMaxLength()));

        // 字符类型规则
        List<CharType> types = new ArrayList<>();
        if (complexityConfig.isRequireLowerCase()) {
            types.add(CharType.LOWER_CASE);
        }
        if (complexityConfig.isRequireUpperCase()) {
            types.add(CharType.UPPER_CASE);
        }
        if (complexityConfig.isRequireNumber()) {
            types.add(CharType.DIGIT);
        }
        if (complexityConfig.isRequireSpecialChar()) {
            types.add(CharType.SPECIAL_CHAR);
        }
        if (complexityConfig.getMinCharTypeCount() > types.size()) {
            throw new IllegalArgumentException("密码字符类型规则配置错误，最小字符类型数不能大于实际字符类型数。");
        }
        if (!types.isEmpty()) {
            ruleManager.addRule(new CharTypePasswordRule(types, complexityConfig.getMinCharTypeCount()));
        }

        // 禁止包含用户信息规则
        if (complexityConfig.isProhibitUserInfo()) {
            ruleManager.addRule(new UserInfoPasswordRule(userInfoList));
        }

        // 禁止全部为单一字符规则
        if (complexityConfig.isProhibitSingleChar()) {
            ruleManager.addRule(new SingleCharPasswordRule());
        }

        // 禁止全部为连续字符规则
        if (complexityConfig.isProhibitConsecutiveChar()) {
            ruleManager.addRule(new ConsecutiveCharPasswordRule());
        }

        // 禁止包含连续字符
        if (complexityConfig.isProhibitContainConsecutiveChar()) {
            ruleManager.addRule(new ContainConsecutiveCharPasswordRule(complexityConfig.getMinConsecutiveCharLength()));
        }

        // 禁止包含连续重复字符
        if (complexityConfig.isProhibitContainRepeatChar()) {
            ruleManager.addRule(new ContainRepeatCharPasswordRule(complexityConfig.getMinRepeatCharLength()));
        }

        // 禁止使用特定密码
        if (complexityConfig.isProhibitSpecificPassword()) {
            ruleManager.addRule(new SpecificPasswordRule(complexityConfig.getProhibitedPasswordList()));
        }
        return ruleManager;
    }
}
