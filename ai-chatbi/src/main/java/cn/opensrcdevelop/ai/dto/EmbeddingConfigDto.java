package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "嵌入模型配置")
@Data
public class EmbeddingConfigDto {

    @Schema(description = "模型提供商ID")
    private String providerId;

    @Schema(description = "相似度阈值")
    private Double similarityThreshold;
}
