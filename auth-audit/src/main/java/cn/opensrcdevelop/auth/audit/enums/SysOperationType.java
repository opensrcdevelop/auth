package cn.opensrcdevelop.auth.audit.enums;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public enum SysOperationType {

    UNKNOWN("未知"), CREATE("创建"), UPDATE("修改"), DELETE("删除");

    private final String name;
}
