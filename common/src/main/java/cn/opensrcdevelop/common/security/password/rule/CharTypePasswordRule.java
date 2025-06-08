package cn.opensrcdevelop.common.security.password.rule;

import cn.opensrcdevelop.common.security.password.CharType;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.stream.Collectors;

@RequiredArgsConstructor
public class CharTypePasswordRule implements PasswordRule {

    private final List<CharType> charTypes;

    private final int minCount;

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        int count = 0;

        if (charTypes.contains(CharType.LETTER) && password.matches(".*[a-zA-Z].*")) {
            count++;
        }

        if (charTypes.contains(CharType.LOWER_CASE) && password.matches(".*[a-z].*")) {
            count++;
        }

        if (charTypes.contains(CharType.UPPER_CASE) && password.matches(".*[A-Z].*")) {
            count++;
        }

        if (charTypes.contains(CharType.DIGIT) && password.matches(".*\\d.*")) {
            count++;
        }

        if (charTypes.contains(CharType.SPECIAL_CHAR) && password.matches(".*[^a-zA-Z0-9\\s].*")) {
            count++;
        }

        return count >= minCount;
    }

    @Override
    public String getRuleName() {
        String names = charTypes.stream().map(CharType::getName).collect(Collectors.joining("、"));
        return String.format("需要至少包含：%s 中的 %s 种。", names, minCount);
    }
}
