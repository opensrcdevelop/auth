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

    @Schema(description = "已用请求令牌数")
    private Long usedReqTokens;

    @Schema(description = "已用响应令牌数")
    private Long usedRepTokens;
}
