package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "示例 SQL 信息")
@Builder
@Getter
@Setter
public class SampleSqlDto {

    @Schema(description = "问题")
    private String question;

    @Schema(description = "生成的 SQL")
    private String sql;
}
