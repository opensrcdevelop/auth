package cn.opensrcdevelop.common.security.password.rule;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;

@RequiredArgsConstructor
public class ContainRepeatCharPasswordRule implements PasswordRule {

    private final int minRepeatCharNum;

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        if (password.length() < minRepeatCharNum) {
            return true;
        }

        int repeatCount = 1;
        for (int i = 1; i < password.length(); i++) {
            if (password.charAt(i) == password.charAt(i - 1)) {
                repeatCount++;
                if (repeatCount >= minRepeatCharNum) {
                    return false;
                }
            } else {
                repeatCount = 1;
            }
        }
        return true;
    }

    @Override
    public String getRuleName() {
        return String.format("禁止包含 %s 位及以上连续重复字符。", minRepeatCharNum);
    }
}
