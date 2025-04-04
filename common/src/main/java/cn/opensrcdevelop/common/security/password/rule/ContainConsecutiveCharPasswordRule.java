package cn.opensrcdevelop.common.security.password.rule;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;

@RequiredArgsConstructor
public class ContainConsecutiveCharPasswordRule implements PasswordRule {

    private final int minConsecutiveCharNum;

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        if (password.length() < minConsecutiveCharNum) {
            return true;
        }

        // 最大连续字符数
        int maxConsecutiveCount = 1;
        int currentConsecutiveCount = 0;
        int diff = 0;

        for (int i = 1; i < password.length(); i++) {
            int currentDiff = password.charAt(i) - password.charAt(i - 1);
            if (currentDiff == 1 || currentDiff == -1) {
                if (currentDiff == diff) {
                    currentConsecutiveCount++;
                } else {
                    currentConsecutiveCount = 2;
                }
                if (currentConsecutiveCount > maxConsecutiveCount) {
                    maxConsecutiveCount = currentConsecutiveCount;
                }
                diff = currentDiff;
            } else {
                currentConsecutiveCount = 1;
                diff = 0;
            }
        }

        return maxConsecutiveCount < minConsecutiveCharNum;
    }

    @Override
    public String getRuleName() {
        return String.format("禁止包含 %s 位及以上连续字符。", minConsecutiveCharNum);
    }
}
