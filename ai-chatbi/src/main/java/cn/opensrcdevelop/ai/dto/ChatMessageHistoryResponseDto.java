package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

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

    @Schema(description = "图表ID")
    private String chartId;

    @Schema(description = "重写后的问题")
    private String rewrittenQuestion;

    @Schema(description = "操作类型")
    private String actionType;

    @Schema(description = "时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime time;
}
