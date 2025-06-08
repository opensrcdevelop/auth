package cn.opensrcdevelop.auth.biz.dto.user.attr;

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
    @EnumValue(value = { "STRING", "BOOLEAN", "NUMBER", "DATETIME", "DATE", "DICT" }, groups = { ValidationGroups.Operation.INSERT.class })
    private String dataType;

    @Schema(description = "扩展属性标记")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    private Boolean extFlg;

    @Schema(description = "是否在用户列表显示")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    private Boolean userLstDisplay;

    @Schema(description = "显示宽度")
    private Integer displayWidth;

    @Schema(description = "用户可见")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    private Boolean userVisible;

    @Schema(description = "用户可编辑")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    private Boolean userEditable;

    @Schema(description = "字典ID")
    private String dictId;
}
