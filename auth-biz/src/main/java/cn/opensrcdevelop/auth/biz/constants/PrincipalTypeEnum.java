package cn.opensrcdevelop.auth.biz.constants;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public enum PrincipalTypeEnum {

    USER("USER", "用户"),

    ROLE("ROLE", "角色"),

    USER_GROUP("USER_GROUP", "用户组");

    private final String type;
    private final String displayName;
}
