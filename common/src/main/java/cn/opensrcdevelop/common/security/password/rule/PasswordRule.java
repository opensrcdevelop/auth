package cn.opensrcdevelop.common.security.password.rule;

public interface PasswordRule {

    boolean validate(String password);

    String getRuleName();
}
