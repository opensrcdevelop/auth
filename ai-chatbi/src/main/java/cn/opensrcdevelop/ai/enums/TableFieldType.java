package cn.opensrcdevelop.ai.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum TableFieldType {

    STRING("字符串"),
    NUMBER("数字"),
    BOOLEAN("布尔值"),
    DATETIME("日期时间");

    private final String displayName;
}
