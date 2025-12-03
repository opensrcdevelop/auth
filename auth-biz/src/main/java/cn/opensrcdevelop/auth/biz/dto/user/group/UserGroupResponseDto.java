package cn.opensrcdevelop.auth.biz.dto.user.group;

import cn.opensrcdevelop.auth.biz.constants.UserGroupType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "用户组响应")
@Data
public class UserGroupResponseDto {

    @Schema(description = "用户组 ID")
    private String id;

    @Schema(description = "用户组名称")
    private String name;

    @Schema(description = "用户组码")
    private String code;

    @Schema(description = "用户组描述")
    private String desc;

    @Schema(description = "成员数")
    private Long memberNum;

    @Schema(description = "用户组类型")
    private UserGroupType type;

    @Schema(description = "动态用户组条件")
    private DynamicUserGroupConditionsDto conditions;
}
