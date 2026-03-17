package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "示例 SQL 嵌入模型配置")
@Data
public class SampleSqlEmbeddingConfigDto {

    @Schema(description = "模型提供商ID")
    @NotBlankStr
    private String providerId;

    @Schema(description = "嵌入模型名称")
    @NotBlankStr
    private String model;

    @Schema(description = "嵌入模型维度")
    private Integer dimension;

    @Schema(description = "相似度阈值")
    private Double similarityThreshold;

    @Schema(description = "上一个嵌入模型名称")
    private String previousModel;

    @Schema(description = "上一个嵌入模型维度")
    private Integer previousDimension;

    @Schema(description = "相似检索返回的最大结果数")
    private Integer topK;
}
