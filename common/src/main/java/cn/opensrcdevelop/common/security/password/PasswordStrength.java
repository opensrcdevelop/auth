package cn.opensrcdevelop.common.security.password;

public enum PasswordStrength {
    NONE, LOW, MEDIUM, HIGH, CUSTOM;

    public static PasswordStrength fromOrdinal(int ordinal) {
        for (PasswordStrength strength : PasswordStrength.values()) {
            if (strength.ordinal() == ordinal) {
                return strength;
            }
        }
        throw new IllegalArgumentException("Invalid ordinal: " + ordinal);
    }
}
