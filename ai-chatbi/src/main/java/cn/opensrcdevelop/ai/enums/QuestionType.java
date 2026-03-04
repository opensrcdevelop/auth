package cn.opensrcdevelop.ai.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum QuestionType {

    TEXT("TEXT", "文本输入"),
    SELECT("SELECT", "单选（支持自定义输入）"),
    MULTI_SELECT("MULTI_SELECT", "多选"),
    DATE("DATE", "日期选择"),
    NUMBER("NUMBER", "数字输入");

    private final String code;
    private final String description;
}
