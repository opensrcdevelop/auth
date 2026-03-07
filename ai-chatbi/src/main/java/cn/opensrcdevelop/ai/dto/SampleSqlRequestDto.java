package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "添加示例 SQL 请求")
@Data
public class SampleSqlRequestDto {

    @NotBlank(message = "数据源ID不能为空")
    @Schema(description = "数据源ID")
    private String dataSourceId;

    @NotBlank(message = "问题不能为空")
    @Schema(description = "问题")
    private String question;

    @NotBlank(message = "SQL不能为空")
    @Schema(description = "SQL")
    private String sql;
}
