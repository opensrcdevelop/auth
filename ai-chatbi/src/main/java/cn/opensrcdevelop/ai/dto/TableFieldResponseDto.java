package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "表字段响应")
@Builder
@Getter
@Setter
public class TableFieldResponseDto {

    @Schema(description = "字段ID")
    private String id;

    @Schema(description = "字段名")
    private String name;

    @Schema(description = "字段类型")
    private String type;

    @Schema(description = "注释")
    private String remark;

    @Schema(description = "是否使用")
    private Boolean toUse;

    @Schema(description = "补充信息")
    private String additionalInfo;
}
