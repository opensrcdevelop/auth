package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "ChatBI 回答响应")
@Builder
@Getter
@Setter
public class ChatAnswerResponseDto {

    @Schema(description = "回答ID")
    private String answerId;

    @Schema(description = "生成的 SQL")
    private String sql;
}
