package cn.opensrcdevelop.auth.biz.constants;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum UserAttrDataTypeEnum {

    /** 字符串类型 */
    STRING("STRING"),

    /** 数字类型 */
    NUMBER("NUMBER"),

    /** 布尔类型 */
    BOOLEAN("BOOLEAN"),

    /** 日期时间类型 */
    DATETIME("DATETIME"),

    /** 日期类型 */
    DATE("DATE"),

    /** 字典类型 */
    DICT("DICT");

    private final String type;
}
