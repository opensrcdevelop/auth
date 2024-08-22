package cn.opensrcdevelop.auth.biz.constants;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum DataFilterEnum {

    /** 等于 */
    EQ("EQ"),

    /** 不等于 */
    NE("NE"),

    /** 类似 */
    LIKE("LIKE"),

    /** 不类似 */
    NOT_LIKE("NOT_LIKE"),

    /** 包含 */
    IN("IN"),

    /** 不包含 */
    NOT_IN("NOT_IN"),

    /** 大于 */
    GT("GT"),

    /** 大于等于 */
    GE("GE"),

    /** 小于 */
    LT("LT"),

    /** 小于等于 */
    LE("LE");

    private final String type;
}
