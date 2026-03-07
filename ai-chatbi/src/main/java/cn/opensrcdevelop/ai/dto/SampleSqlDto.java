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

    @Schema(description = "ID")
    private String id;

    @Schema(description = "回答ID")
    private String answerId;

    @Schema(description = "问题")
    private String question;

    @Schema(description = "SQL")
    private String sql;

    @Schema(description = "数据源ID")
    private String dataSourceId;

    @Schema(description = "创建时间")
    private String createdAt;
}
