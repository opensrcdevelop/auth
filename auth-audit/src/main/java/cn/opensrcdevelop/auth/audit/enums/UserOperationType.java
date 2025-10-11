package cn.opensrcdevelop.auth.audit.enums;

import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public enum UserOperationType {

    UNKNOWN("未知"),
    UPDATE_USER_INFO("修改个人信息"),
    UPDATE_PWD("修改密码"),
    BIND_EMAIL("绑定邮箱"),
    UNBIND_EMAIL("解绑邮箱"),
    BIND_PHONE("绑定手机号"),
    UNBIND_PHONE("解绑手机号"),
    BIND_THIRD_ACCOUNT("绑定第三方账号"),
    UNBIND_THIRD_ACCOUNT("解绑第三方账号"),
    BIND_MFA("绑定 MFA 设备"),
    RESET_PWD("重置密码"),
    CHAT_BI_CHAT("ChatBI 对话"),
    CHAT_BI_VOTE("ChatBI 问答反馈"),
    CHAT_BI_DELETE_HISTORY("ChatBI 删除对话历史"),
    CHAT_BI_UPDATE_HISTORY("ChatBI 更新对话历史");

    private final String name;
}
