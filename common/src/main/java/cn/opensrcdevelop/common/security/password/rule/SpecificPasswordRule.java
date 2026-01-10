package cn.opensrcdevelop.common.security.password.rule;

import java.util.List;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;

@RequiredArgsConstructor
public class SpecificPasswordRule implements PasswordRule {

    private final List<String> specificPasswords;

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        return specificPasswords.stream().noneMatch(password::equals);
    }

    @Override
    public String getRuleName() {
        return "禁止使用特定密码。";
    }
}
