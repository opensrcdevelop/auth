package cn.opensrcdevelop.auth.audit.enums;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public enum AuditType {

    SYS_OPERATION("系统操作"), USER_OPERATION("用户操作");

    private final String name;
}
