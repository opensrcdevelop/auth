package cn.opensrcdevelop.auth.biz.constants;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum AuthorizeTypeEnum {

    SYSTEM_DEFAULT("SYSTEM_DEFAULT", "系统默认"),
    ADMINISTRATOR_GRANT("ADMINISTRATOR_GRANT", "管理员赋予"),
    ADMINISTRATOR_APPROVE("ADMINISTRATOR_APPROVE", "管理员审批"),
    AUTO_APPROVE("AUTO_APPROVE", "自动审批");

    private final String type;
    private final String displayName;

    public static AuthorizeTypeEnum fromType(String type) {
        for (AuthorizeTypeEnum authorizeTypeEnum : values()) {
            if (authorizeTypeEnum.getType().equals(type)) {
                return authorizeTypeEnum;
            }
        }

        return null;
    }
}
