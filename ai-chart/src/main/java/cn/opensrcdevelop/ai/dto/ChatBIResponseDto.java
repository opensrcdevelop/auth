package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.ai.enums.ChatContentType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "ChatBI 响应")
@Builder
@Getter
@Setter
public class ChatBIResponseDto {

    @Schema(description = "内容")
    private Object content;

    @Schema(description = "类型")
    private ChatContentType type;

    @Schema(description = "问题ID")
    private String questionId;

    @Schema(description = "对话ID")
    private String chatId;
}