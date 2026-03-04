package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "用户响应请求")
@Data
public class UserResponseRequestDto {

    @Schema(description = "对话ID")
    @NotBlank
    private String chatId;

    @Schema(description = "用户回答")
    @NotBlank
    private String answer;

    @Schema(description = "问题ID（用于关联具体问题）")
    private String questionId;
}
