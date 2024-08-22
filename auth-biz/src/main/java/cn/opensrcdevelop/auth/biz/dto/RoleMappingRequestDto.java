package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

@Schema(description = "创建 / 更新角色映射请求")
@Data
public class RoleMappingRequestDto {

    @Schema(description = "用户ID集合")
    private List<String> userIds;

    @Schema(description = "用户组ID集合")
    private List<String> userGroupIds;

    @Schema(description = "角色ID")
    @NotEmpty
    private List<String> roleIds;
}
