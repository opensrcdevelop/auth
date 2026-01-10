package cn.opensrcdevelop.auth.biz.dto.user.attr;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "创建 / 更新用户属性映射关系请求")
@Data
public class UserAttrMappingRequestDto {

    @Schema(description = "属性 ID")
    @NotBlank
    private String attrId;

    @Schema(description = "属性值")
    @NotNull(groups = {ValidationGroups.Operation.INSERT.class})
    private String attrValue;
}
