package cn.opensrcdevelop.common.security.password;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum CharType {

    LETTER("英文字母"),
    LOWER_CASE("小写字母"),
    UPPER_CASE("大写字母"),
    DIGIT("数字"),
    SPECIAL_CHAR("特殊字符");

    private final String name;
}