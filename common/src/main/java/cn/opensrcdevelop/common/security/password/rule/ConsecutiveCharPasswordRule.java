package cn.opensrcdevelop.common.security.password.rule;

import org.apache.commons.lang3.StringUtils;

public class ConsecutiveCharPasswordRule implements PasswordRule {

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        if (password.length() < 2) {
            return true;
        }

        boolean isConsecutive = true;
        int diff = password.charAt(1) - password.charAt(0);
        if (diff != 1 && diff != -1) {
            isConsecutive = false;
        }

        for (int i = 1; i < password.length(); i++) {
            if (password.charAt(i) - password.charAt(i - 1) != diff) {
                isConsecutive = false;
                break;
            }
        }
        return !isConsecutive;
    }

    @Override
    public String getRuleName() {
        return "禁止全部连续字符。";
    }
}
