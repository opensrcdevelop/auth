package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.ai.enums.ChatContentType;
import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

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

    @Schema(description = "回答ID")
    private String answerId;

    @Schema(description = "重写后的问题")
    private String rewrittenQuestion;

    @Schema(description = "时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime time;
}