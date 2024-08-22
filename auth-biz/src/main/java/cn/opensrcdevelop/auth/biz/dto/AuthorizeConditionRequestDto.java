package cn.opensrcdevelop.auth.biz.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.util.List;

@Schema(description = "添加 / 删除授权条件请求")
@Data
public class AuthorizeConditionRequestDto {

    @Schema(description = "授权ID集合")
    @NotEmpty
    private List<String> authorizeIds;

    @Schema(description = "权限表达式ID集合")
    @NotEmpty
    private List<String> permissionExpIds;
}
