package cn.opensrcdevelop.auth.biz.entity.asynctask;

import cn.opensrcdevelop.common.entity.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import java.time.LocalDateTime;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 异步任务实体
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("t_async_task")
public class AsyncTask extends BaseEntity {

    /**
     * 任务ID
     */
    @TableId
    private String taskId;

    /**
     * 任务类型（如：USER_IMPORT, USER_EXPORT）
     */
    private String taskType;

    /**
     * 任务名称
     */
    private String taskName;

    /**
     * 任务状态：PENDING-等待中, RUNNING-执行中, SUCCESS-成功, FAILED-失败, CANCELLED-已取消
     */
    private String status;

    /**
     * 任务参数（JSON格式）
     */
    private String taskParams;

    /**
     * 任务结果（JSON格式）
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
    private LocalDateTime startTime;

    /**
     * 任务结束时间
     */
    private LocalDateTime endTime;

    /**
     * 任务执行耗时（毫秒）
     */
    private Long duration;

    /**
     * 用户ID（用于区分任务创建者）
     */
    private String userId;
}
