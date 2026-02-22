package cn.opensrcdevelop.auth.biz.dto.asynctask;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * 异步任务列表 DTO（精简版）
 */
@Data
public class AsyncTaskListDto {

    private String taskId;

    private String taskType;

    private String taskName;

    private String status;

    private Integer progress;

    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime startTime;

    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime endTime;

    private Long duration;

    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime createTime;
}
