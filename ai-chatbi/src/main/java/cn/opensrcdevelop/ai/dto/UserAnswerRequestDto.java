package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.List;

@Schema(description = "回答 AI 对用户的提问请求")
@Data
public class UserAnswerRequestDto implements Serializable {

    @Serial
    private static final long serialVersionUID = 7275889512721669008L;

    @Schema(description = "对话ID")
    @NotBlank
    private String chatId;

    @Schema(description = "用户回答列表（支持多个问题）")
    private List<@Valid UserAnswerDto> answers;
}
