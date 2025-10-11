package cn.opensrcdevelop.ai.dto;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;

@Schema(description = "对话历史响应")
@Builder
@Setter
@Getter
public class ChatHistoryResponseDto {

    @Schema(description = "对话ID")
    private String id;

    @Schema(description = "标题")
    private String title;

    @Schema(description = "描述")
    private String desc;

    @Schema(description = "数据源ID")
    private String dataSourceId;

    @Schema(description = "开始时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime start;

    @Schema(description = "结束时间")
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime end;
}
