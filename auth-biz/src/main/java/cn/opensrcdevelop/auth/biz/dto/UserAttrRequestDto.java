package cn.opensrcdevelop.auth.biz.dto;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.EnumValue;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Schema(description = "创建 / 更新用户属性请求")
@Data
public class UserAttrRequestDto {

    @Schema(description = "属性 ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "属性键")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    private String key;

    @Schema(description = "属性名")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "属性数据类型")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    @EnumValue(value = { "STRING", "BOOLEAN", "NUMBER", "DATETIME" }, groups = { ValidationGroups.Operation.INSERT.class })
    private String dataType;

    @Schema(description = "扩展属性标记")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    private Boolean extFlg;

    @Schema(description = "是否在用户列表显示")
    @NotNull
    private Boolean userLstDisplay;

    @Schema(description = "显示宽度")
    private Integer displayWidth;
}
