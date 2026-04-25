package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "添加示例 SQL 请求")
@Data
public class SampleSqlRequestDto {

    @Schema(description = "数据源ID")
    @NotBlank
    private String dataSourceId;

    @Schema(description = "问题")
    @NotBlank
    private String question;

    @Schema(description = "SQL")
    @NotBlank
    private String sql;
}
