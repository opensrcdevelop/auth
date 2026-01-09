package cn.opensrcdevelop.auth.biz.dto.user.attr.dict;

import cn.opensrcdevelop.common.validation.ValidationGroups;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "添加 / 删除子字典请求参数")
@Data
public class ChildDictRequestDto {

    @Schema(description = "字典ID")
    @NotBlank
    private String id;

    @Schema(description = "关联的父字典数据ID")
    @NotBlank(groups = ValidationGroups.Operation.INSERT.class)
    private String dataId;

    @Schema(description = "子字典ID")
    @NotBlank
    private String childId;
}
