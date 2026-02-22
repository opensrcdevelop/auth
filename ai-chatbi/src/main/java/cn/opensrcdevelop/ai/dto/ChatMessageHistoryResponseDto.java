package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import java.time.LocalDateTime;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Schema(description = "对话消息历史响应")
@Builder
@Setter
@Getter
public class ChatMessageHistoryResponseDto {

    @Schema(description = "消息ID")
    private String id;

    @Schema(description = "角色")
    private String role;

    @Schema(description = "消息类型")
    private String type;

    @Schema(description = "内容")
    private Object content;

    @Schema(description = "问题ID")
    private String questionId;

    @Schema(description = "对话ID")
    private String chatId;

    @Schema(description = "回答ID")
    private String answerId;

    @Schema(description = "重写后的问题")
    private String rewrittenQuestion;

    @Schema(description = "用户反馈")
    private String feedback;

    @Schema(description = "时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime time;
}
