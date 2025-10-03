package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Schema(description = "模型提供商响应")
@Builder
@Getter
@Setter
public class ModelProviderResponseDto {

    @Schema(description = "模型提供商ID")
    private String id;

    @Schema(description = "模型提供商名称")
    private String name;

    @Schema(description = "模型提供商类型")
    private String type;

    @Schema(description = "模型提供商基础URL")
    private String baseUrl;

    @Schema(description = "模型提供商API Key")
    private String apiKey;

    @Schema(description = "可选模型响应")
    private List<ModelResponseDto> optionalModels;

    @Schema(description = "模型提供商默认模型")
    private String defaultModel;

    @Schema(description = "模型提供商温度")
    private Double temperature;

    @Schema(description = "模型提供商最大令牌数")
    private Integer maxTokens;

    @Schema(description = "模型提供商是否启用")
    private Boolean enabled;
}
