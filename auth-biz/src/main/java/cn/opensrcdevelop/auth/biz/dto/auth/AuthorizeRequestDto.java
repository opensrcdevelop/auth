package cn.opensrcdevelop.auth.biz.dto.auth;

import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

@Schema(description = "授权请求")
@Data
public class AuthorizeRequestDto {

    @Schema(description = "用户ID")
    private List<String> userIds;

    @Schema(description = "角色ID")
    private List<String> roleIds;

    @Schema(description = "用户组ID")
    private List<String> userGroupIds;

    @Schema(description = "资源ID（鉴权用）")
    private String resourceId;

    @Schema(description = "权限ID集合")
    @NotEmpty
    private List<String> permissionIds;

    @Schema(description = "限定条件集合")
    private List<String> expressionIds;

    @Schema(description = "优先级")
    @EnumValue({ "-1", "0", "1", "2", "3" })
    private  Integer priority;
}
