package cn.opensrcdevelop.auth.biz.dto.user.group;

import cn.opensrcdevelop.auth.biz.constants.UserGroupType;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.AlphaNum;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "创建 / 更新用户组请求")
@Data
public class UserGroupRequestDto {

    @Schema(description = "用户组ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "用户组名")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "用户组标识")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    @AlphaNum
    private String code;

    @Schema(description = "描述")
    private String desc;

    @Schema(description = "用户组类型")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    private UserGroupType type;

    @Schema(description = "动态用户组条件")
    @NotNull(groups = { DynamicUserGroup.class })
    private DynamicUserGroupConditionsDto conditions;

    public static interface DynamicUserGroup {}

    public static interface StaticUserGroup {}
}
