package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "表字段请求")
@Data
public class TableFieldRequestDto {

    @Schema(description = "表字段ID")
    @NotBlank
    private String id;

    @Schema(description = "注释")
    public String remark;

    @Schema(description = "是否使用")
    public Boolean toUse;

    @Schema(description = "补充信息")
    private String additionalInfo;
}
