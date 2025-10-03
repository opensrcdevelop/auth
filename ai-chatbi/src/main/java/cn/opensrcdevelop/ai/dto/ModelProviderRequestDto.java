package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.ai.enums.ModelProviderType;
import cn.opensrcdevelop.common.validation.ValidationGroups;
import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.List;

@Schema(description = "模型提供商请求")
@Data
public class ModelProviderRequestDto {

    @Schema(description = "模型提供商ID")
    @NotBlank(groups = { ValidationGroups.Operation.UPDATE.class })
    private String id;

    @Schema(description = "模型提供商名称")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String name;

    @Schema(description = "模型提供商类型")
    @NotNull(groups = { ValidationGroups.Operation.INSERT.class })
    private ModelProviderType type;

    @Schema(description = "模型提供商基础URL")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String baseUrl;

    @Schema(description = "模型提供商API Key")
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String apiKey;

    @Schema(description = "可选模型")
    @NotEmpty(groups = { ValidationGroups.Operation.INSERT.class })
    private List<@NotBlankStr String> optionalModels;

    @Schema(description = "模型提供商默认模型")
    @NotBlank(groups = { ValidationGroups.Operation.INSERT.class })
    @NotBlankStr(groups = { ValidationGroups.Operation.UPDATE.class })
    private String defaultModel;

    @Schema(description = "模型提供商温度")
    private Double temperature;

    @Schema(description = "模型提供商最大令牌数")
    private Integer maxTokens;

    @Schema(description = "模型提供商是否启用")
    private Boolean enabled;
}
