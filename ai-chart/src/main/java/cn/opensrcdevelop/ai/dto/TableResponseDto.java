package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "表响应")
@Builder
@Getter
@Setter
public class TableResponseDto {

    @Schema(description = "表ID")
    private String id;

    @Schema(description = "模式")
    private String schema;

    @Schema(description = "表名")
    private String name;

    @Schema(description = "注释")
    private String remark;

    @Schema(description = "是否使用")
    private Boolean toUse;

    @Schema(description = "补充信息")
    private String additionalInfo;
}
