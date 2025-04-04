package cn.opensrcdevelop.common.security.password.rule;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;

@RequiredArgsConstructor
public class LengthPasswordRule implements PasswordRule {

    // 最小长度
    private final int minLength;

    // 最大长度（-1 无限制）
    private final int maxLength;

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        if (maxLength == -1) {
            return password.length() >= minLength;
        }

        return password.length() >= minLength && password.length() <= maxLength;
    }

    @Override
    public String getRuleName() {
        return String.format("长度要求 %s 位及以上，%s 位及以下。", minLength, maxLength);
    }
}
