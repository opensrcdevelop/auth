package cn.opensrcdevelop.auth.biz.dto.user.group;

import cn.opensrcdevelop.auth.biz.constants.ConjunctionType;
import cn.opensrcdevelop.auth.biz.dto.user.DataFilterDto;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;

@Schema(description = "动态用户组条件请求/响应")
@Data
public class DynamicUserGroupConditionsDto {

    @Schema(description = "过滤条件")
    private List<@Valid DataFilterDto> filters;

    @Schema(description = "连接类型")
    @NotNull
    private ConjunctionType conjunction;

    @Schema(description = "子组条件")
    private List<@Valid DynamicUserGroupConditionsDto> groups;
}
