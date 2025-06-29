package cn.opensrcdevelop.auth.audit.enums;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public enum UserOperationType {

    UNKNOWN("未知"),
    UPDATE_USER_INFO("修改用户信息"),
    UPDATE_PWD("修改密码"),
    BIND_EMAIL("绑定邮箱"),
    UNBIND_EMAIL("解绑邮箱"),
    BIND_PHONE("绑定手机号"),
    UNBIND_PHONE("解绑手机号"),
    BIND_THIRD_ACCOUNT("绑定第三方账号"),
    UNBIND_THIRD_ACCOUNT("解绑第三方账号"),
    BIND_MFA("绑定 MFA"),
    RESET_PWD("重置密码");

    private final String name;
}
