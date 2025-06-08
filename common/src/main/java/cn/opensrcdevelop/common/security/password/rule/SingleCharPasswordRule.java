package cn.opensrcdevelop.common.security.password.rule;

import org.apache.commons.lang3.StringUtils;

public class SingleCharPasswordRule implements PasswordRule {

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        return !password.chars().allMatch(c -> c == password.charAt(0));
    }

    @Override
    public String getRuleName() {
        return "禁止全部单一字符。";
    }
}
