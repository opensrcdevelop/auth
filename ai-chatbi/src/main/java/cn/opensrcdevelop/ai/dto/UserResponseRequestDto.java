package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.util.List;
import java.util.Map;

@Schema(description = "用户响应请求")
@Data
public class UserResponseRequestDto {

    @Schema(description = "对话ID")
    @NotBlank
    private String chatId;

    @Schema(description = "用户回答列表（支持多个问题）")
    private List<Map<String, Object>> answers;
}
