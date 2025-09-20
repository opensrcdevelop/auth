package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "表请求")
@Data
public class TableRequestDto {

    @Schema(description = "表ID")
    @NotBlank
    public String id;

    @Schema(description = "表名")
    public String name;

    @Schema(description = "注释")
    public String remark;

    @Schema(description = "是否使用")
    public Boolean toUse;

    @Schema(description = "补充信息")
    private String additionalInfo;
}
