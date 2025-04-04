package cn.opensrcdevelop.common.security.password.strength;

public interface PasswordStrengthChecker {

    Boolean validate(String password);

    String errorMessage();
}
