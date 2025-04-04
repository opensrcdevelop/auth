package cn.opensrcdevelop.common.security.password;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@Data
public class PasswordComplexityConfig implements Serializable {

    @Serial
    private static final long serialVersionUID = -421882870879601917L;

    /** 最小长度 */
    private int minLength;

    /** 最大长度 */
    private int maxLength;

    /** 是否必须包含数字 */
    private boolean requireNumber;

    /** 是否必须包含小写字母 */
    private boolean requireLowerCase;

    /** 是否必须包含大写字母 */
    private boolean requireUpperCase;

    /** 是否必须包含特殊字符 */
    private boolean requireSpecialChar;

    /** 至少需要满足的字符类型数量 */
    private int minCharTypeCount;

    /** 是否禁止包含用户信息 */
    private boolean prohibitUserInfo;

    /** 是否禁止全部为单一字符 */
    private boolean prohibitSingleChar;

    /** 是否禁止全部为连续字符 */
    private boolean prohibitConsecutiveChar;

    /** 是否禁止包含连续字符 */
    private boolean prohibitContainConsecutiveChar;

    /** 禁止包含的连续字符的最小长度 */
    private int minConsecutiveCharLength;

    /** 是否禁止包含连续重复字符 */
    private boolean prohibitContainRepeatChar;

    /** 禁止包含的连续重复字符的最小长度 */
    private int minRepeatCharLength;

    /** 是否禁止使用特定密码 */
    private boolean prohibitSpecificPassword;

    /** 禁止使用的特定密码列表 */
    private List<String> prohibitedPasswordList;
}
