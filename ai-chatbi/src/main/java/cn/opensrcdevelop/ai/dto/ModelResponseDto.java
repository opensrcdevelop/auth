package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "模型响应")
@Setter
@Getter
@Builder
public class ModelResponseDto {

    @Schema(description = "模型名称")
    private String name;

    @Schema(description = "已用输入 token 数")
    private Long usedInputTokens;

    @Schema(description = "已用输出 token 数")
    private Long usedOutputTokens;
}
