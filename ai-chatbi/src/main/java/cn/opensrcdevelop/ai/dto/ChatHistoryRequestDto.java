package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.validation.constraints.NotBlankStr;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Schema(description = "对话历史请求")
@Data
public class ChatHistoryRequestDto {

    @Schema(description = "对话ID")
    @NotBlank
    private String id;

    @Schema(description = "标题")
    @NotBlankStr
    private String title;

    @Schema(description = "描述")
    @NotBlankStr
    private String desc;
}
