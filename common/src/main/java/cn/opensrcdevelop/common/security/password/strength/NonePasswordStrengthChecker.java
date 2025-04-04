package cn.opensrcdevelop.common.security.password.strength;

public class NonePasswordStrengthChecker implements PasswordStrengthChecker {

    @Override
    public Boolean validate(String password) {
        return Boolean.TRUE;
    }

    @Override
    public String errorMessage() {
        return "";
    }
}
