package cn.opensrcdevelop.auth.biz.dto.user.attr.dict;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "创建 / 更新字典数据")
@Data
public class DictDataRequestDto {

    @Schema(description = "字典ID")
    @NotBlank
    private String dictId;

    @Schema(description = "字典数据ID")
    @NotBlank(groups = {ValidationGroups.Operation.UPDATE.class})
    private String id;

    @Schema(description = "字典数据标签")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String label;

    @Schema(description = "字典数据值")
    @NotBlank(groups = {ValidationGroups.Operation.INSERT.class})
    @NotBlankStr(groups = {ValidationGroups.Operation.UPDATE.class})
    private String value;

    @Schema(description = "状态：true - 启用、false - 禁用")
    private Boolean enable;

    @Schema(description = "显示顺序")
    @Min(1)
    private Integer displaySeq;
}
