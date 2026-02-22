package cn.opensrcdevelop.auth.biz.service.asynctask;

import cn.opensrcdevelop.auth.biz.dto.asynctask.AsyncTaskResponseDto;
import cn.opensrcdevelop.common.response.PageData;

/**
 * 异步任务服务接口
 */
public interface AsyncTaskService {

    /**
     * 创建异步任务
     *
     * @param taskType
     *            任务类型
     * @param taskName
     *            任务名称
     * @param taskParams
     *            任务参数（JSON格式）
     * @param userId
     *            用户ID
     * @return 任务ID
     */
    String createTask(String taskType, String taskName, String taskParams, String userId);

    /**
     * 更新任务状态为执行中
     *
     * @param taskId
     *            任务ID
     */
    void startTask(String taskId);

    /**
     * 更新任务状态为成功
     *
     * @param taskId
     *            任务ID
     * @param taskResult
     *            任务结果
     * @param resultFilePath
     *            结果文件路径
     * @param resultFileName
     *            结果文件名
     */
    void completeTaskSuccess(String taskId, String taskResult, String resultFilePath, String resultFileName);

    /**
     * 更新任务状态为失败
     *
     * @param taskId
     *            任务ID
     * @param errorMessage
     *            错误信息
     */
    void completeTaskFailed(String taskId, String errorMessage);

    /**
     * 更新任务进度
     *
     * @param taskId
     *            任务ID
     * @param progress
     *            进度（0-100）
     */
    void updateProgress(String taskId, Integer progress);

    /**
     * 取消任务
     *
     * @param taskId
     *            任务ID
     */
    void cancelTask(String taskId);

    /**
     * 根据任务ID查询任务详情
     *
     * @param taskId
     *            任务ID
     * @return 任务详情
     */
    AsyncTaskResponseDto getTaskDetailById(String taskId);

    /**
     * 分页查询任务列表
     *
     * @param page
     *            页码
     * @param pageSize
     *            每页数量
     * @param taskType
     *            任务类型（可选）
     * @param status
     *            任务状态（可选）
     * @param userId
     *            用户ID（可选）
     * @return 分页数据
     */
    PageData<AsyncTaskResponseDto> listTasks(int page, int pageSize, String taskType, String status, String userId);

    /**
     * 更新任务状态
     *
     * @param taskId
     *            任务ID
     * @param status
     *            新状态
     */
    void updateTaskStatus(String taskId, String status);

    /**
     * 查询指定类型正在运行的任务数量
     *
     * @param taskType
     *            任务类型
     * @return 正在运行的任务数量
     */
    long countRunningTasks(String taskType);
}
