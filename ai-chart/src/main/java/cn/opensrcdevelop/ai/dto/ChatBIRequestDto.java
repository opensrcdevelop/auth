package cn.opensrcdevelop.ai.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Schema(description = "ChatBI 请求")
@Data
public class ChatBIRequestDto {

    @Schema(description = "对话ID")
    private String chatId;

    @Schema(description = "问题ID")
    private String questionId;

    @Schema(description = "模型提供商ID")
    private String modelProviderId;

    @Schema(description = "模型")
    private String model;

    @Schema(description = "数据源ID")
    private String dataSourceId;

    @Schema(description = "问题")
    private String question;
}
