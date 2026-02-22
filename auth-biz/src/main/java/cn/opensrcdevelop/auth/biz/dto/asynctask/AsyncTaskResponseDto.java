package cn.opensrcdevelop.auth.biz.dto.asynctask;

import cn.opensrcdevelop.common.constants.CommonConstants;
import com.fasterxml.jackson.annotation.JsonFormat;
import java.time.LocalDateTime;
import lombok.Data;

/**
 * 异步任务响应 DTO
 */
@Data
public class AsyncTaskResponseDto {

    /**
     * 任务ID
     */
    private String taskId;

    /**
     * 任务类型
     */
    private String taskType;

    /**
     * 任务名称
     */
    private String taskName;

    /**
     * 任务状态
     */
    private String status;

    /**
     * 任务参数
     */
    private String taskParams;

    /**
     * 任务结果
     */
    private String taskResult;

    /**
     * 结果文件路径
     */
    private String resultFilePath;

    /**
     * 结果文件名称
     */
    private String resultFileName;

    /**
     * 错误信息
     */
    private String errorMessage;

    /**
     * 进度百分比（0-100）
     */
    private Integer progress;

    /**
     * 任务开始时间
     */
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime startTime;

    /**
     * 任务结束时间
     */
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime endTime;

    /**
     * 任务执行耗时（毫秒）
     */
    private Long duration;

    /**
     * 用户ID
     */
    private String userId;

    /**
     * 创建时间
     */
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime createTime;

    /**
     * 更新时间
     */
    @JsonFormat(pattern = CommonConstants.LOCAL_DATETIME_FORMAT_YYYYMMDDHHMMSS)
    private LocalDateTime updateTime;
}
