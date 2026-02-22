package cn.opensrcdevelop.common.security.password.rule;

import io.vavr.Tuple;
import io.vavr.Tuple2;
import java.util.ArrayList;
import java.util.List;

public class PasswordRuleManager {

    private final List<PasswordRule> rules = new ArrayList<>();

    /**
     * 添加密码规则
     *
     * @param rule
     *            密码规则
     */
    public void addRule(PasswordRule rule) {
        rules.add(rule);
    }

    /**
     * 密码规则校验
     *
     * @param password
     *            密码
     * @return 校验结果
     */
    public List<Tuple2<String, Boolean>> validate(String password) {
        return rules.stream().map(rule -> Tuple.of(rule.getRuleName(), rule.validate(password))).toList();
    }

    /**
     * 密码规则校验
     *
     * @param password
     *            密码
     * @return 校验结果
     */
    public Boolean validateAll(String password) {
        return validate(password).stream().allMatch(Tuple2::_2);
    }
}
