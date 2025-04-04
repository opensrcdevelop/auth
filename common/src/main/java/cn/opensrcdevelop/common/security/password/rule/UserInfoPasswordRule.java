package cn.opensrcdevelop.common.security.password.rule;

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;

import java.util.List;

@RequiredArgsConstructor
public class UserInfoPasswordRule implements PasswordRule {

    private final List<String> userInfoList;

    @Override
    public boolean validate(String password) {
        if (StringUtils.isEmpty(password)) {
            return false;
        }

        return userInfoList.stream().noneMatch(password::contains);
    }

    @Override
    public String getRuleName() {
        return "禁止包含用户信息。";
    }
}
